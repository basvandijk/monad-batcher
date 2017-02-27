{-# language ExistentialQuantification #-}
{-# language RankNTypes #-}

module Control.Monad.Batcher
  ( Batcher
  , schedule
  , runBatcher
  , catchBatcher
  , Worker
  , Scheduled(..)
  , simpleWorker
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow, throwM, MonadCatch, catch, Exception, SomeException, try)
import Data.Foldable (traverse_)
import Data.Functor ((<$),($>))
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

newtype Batcher command m a = Batcher
    { unBatcher :: forall b
                 . IORef [Scheduled command m]
                -> Done m a b
                -> Blocked command m a b
                -> m b
    }

type Done m a b = a -> m b
type Blocked command m a b = Batcher command m a -> m b

runBatcher
    :: forall command m a
     . (MonadIO m)
    => Worker command m
    -> Batcher command m a
    -> m a
runBatcher work batcher = do
    ref <- liftIO $ newIORef []
    let run (Batcher b) = b ref pure blocked
        blocked b = do
          scheduledCmds <- liftIO $ readIORef ref
          liftIO $ writeIORef ref []
          work $ reverse scheduledCmds
          run b
    run batcher

instance Functor (Batcher command m) where
    {-# INLINABLE fmap #-}
    fmap f (Batcher b) = Batcher $ \ref done blocked ->
                           b ref (done . f)
                                 (blocked . fmap f)

    {-# INLINABLE (<$) #-}
    y <$ Batcher b = Batcher $ \ref done blocked ->
                       b ref (\_x -> done y)
                             (blocked . (y <$))

instance Applicative (Batcher command m) where
    {-# INLINABLE pure #-}
    pure x = Batcher $ \_ref done _blocked -> done x

    {-# INLINABLE (<*>) #-}
    Batcher bF <*> Batcher bX = Batcher $ \ref done blocked ->
        let doneF f = bX ref (done . f) (blocked . fmap f)
            blockedF bF' = let doneX     x  = blocked (bF' <&> ($ x))
                               blockedX bX' = blocked (bF' <*> bX')
                           in bX ref doneX blockedX
        in bF ref doneF blockedF

    {-# INLINABLE (*>) #-}
    Batcher bY *> Batcher bX = Batcher $ \ref done blocked ->
        let doneY _y = bX ref done blocked
            blockedY bY' = let doneX     x  = blocked (bY' $>  x)
                               blockedX bX' = blocked (bY' *> bX')
                           in bX ref doneX blockedX
        in bY ref doneY blockedY

    {-# INLINABLE (<*) #-}
    Batcher bX <* Batcher bY = Batcher $ \ref done blocked ->
        let doneX x = bY ref (\_y -> done x)
                             (\bY' -> blocked $ x <$ bY')
            blockedX bX' = let doneY    _y  = blocked bX'
                               blockedY bY' = blocked (bX' <* bY')
                           in bY ref doneY blockedY
        in bX ref doneX blockedX

instance Monad (Batcher command m) where
    return = pure

    {-# INLINABLE (>>=) #-}
    Batcher b >>= f = Batcher $ \ref done blocked ->
        b ref (\x -> unBatcher (f x) ref done blocked)
              (\b' -> blocked (b' >>= f))

    (>>) = (*>)

catchBatcher
    :: (MonadCatch m, Exception e)
    => Batcher command m a
    -> (e -> Batcher command m a)
    -> Batcher command m a
Batcher b `catchBatcher` h = Batcher $ \ref done blocked ->
    let blockedInCatch b' = blocked $ b' `catchBatcher` h
    in b ref done blockedInCatch
         `catch` \e -> unBatcher (h e) ref done blocked

schedule :: (MonadIO m, MonadThrow m) => command a -> Batcher command m a
schedule cmd = Batcher $ \ref _done blocked -> do
    resultMVar <- liftIO $ do
      scheduledCmds <- readIORef ref
      resultMVar <- newEmptyMVar
      let write r = liftIO $ putMVar resultMVar r
      writeIORef ref (Scheduled cmd write : scheduledCmds)
      pure resultMVar
    blocked $ Batcher $ \_ref done _blocked -> do
      r <- liftIO $ takeMVar resultMVar
      case r of
        Left ex -> throwM ex
        Right x -> done x

-- | A @Worker@ is responsible for executing the given batch of scheduled
-- commands. Instead of executing each command individually it might group
-- commands and execute each group in one go. It might also execute each command
-- concurrently.
--
-- The @Worker@ should ensure that the result of /each/ command is written using
-- 'writeResult'.
type Worker command m = [Scheduled command m] -> m ()

-- | A @'schedule'd@ command paired with a function to communicate its result.
--
-- Note that the result of the command is
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#ghc-flag--XExistentialQuantification existentially quantified>.
-- This ensures that multiple commands with different
-- result types can be batched in a list which can then be given to a 'Worker' for execution.
data Scheduled command m = forall a.
     Scheduled
     { command     :: command a
     , writeResult :: Either SomeException a -> m ()
     }

-- | A convenience @Worker@ that simply executes commands using the given
-- function without any batching.
simpleWorker :: (MonadCatch m) => (forall r. command r -> m r) -> Worker command m
simpleWorker exe = traverse_ $ \(Scheduled cmd write) -> try (exe cmd) >>= write

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap
