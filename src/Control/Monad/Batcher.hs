{-# language ExistentialQuantification #-}
{-# language RankNTypes #-}

module Control.Monad.Batcher
  ( Batcher

  , schedule
  , schedule_

  , runBatcher
  , Worker
  , SomeCommand(..)
  , simpleWorker
  ) where

import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)

-- | An applicative monad that schedules commands for later more efficient execution.
newtype Batcher command m a = Batcher
    { unBatcher :: IORef [SomeCommand command m] -> m (Result command m a) }

data Result command m a
   = Done a
   | Blocked (Batcher command m a)

instance (Functor m) => Functor (Result command m) where
    fmap f (Done     x) = Done    $ f      x
    fmap f (Blocked bx) = Blocked $ f <$> bx

instance (Applicative m) => Applicative (Result command m) where
    pure = Done
    Done     f <*> Done     x = Done    $  f        x
    Done     f <*> Blocked bx = Blocked $  f <$>   bx
    Blocked bf <*> Done     x = Blocked $ bf <&> ($ x)
    Blocked bf <*> Blocked bx = Blocked $ bf <*>   bx

instance (Functor m) => Functor (Batcher command m) where
    fmap f b = Batcher $ \ref -> fmap (f <$>) (unBatcher b ref)

instance (Applicative m) => Applicative (Batcher command m) where
    pure    x = Batcher $ \_ref -> pure $ pure x
    bf <*> bx = Batcher $ \ ref -> liftA2 (<*>) (unBatcher bf ref) (unBatcher bx ref)

instance (Monad m) => Monad (Batcher command m) where
    return = pure
    bx >>= f = Batcher $ \ref -> do
                 rx <- unBatcher bx ref
                 case rx of
                   Done     x  -> unBatcher (f x) ref
                   Blocked bx' -> pure $ Blocked $ bx' >>= f
    (>>) = (*>)

-- | A command paired with a function to communicate its result.
--
-- Note that the result of the command is
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#ghc-flag--XExistentialQuantification existentially quantified>.
-- This ensures that multiple commands with different
-- result types can be stored in a list which can then be given to a 'Worker' for execution.
data SomeCommand command m = forall r.
     SomeCommand
     { command     :: command r
     , writeResult :: r -> m ()
     }

-- | Schedule a command for later execution.
--
-- For commands that have no result it's more efficient to use 'schedule_'.
schedule :: (MonadIO m) => command a -> Batcher command m a
schedule cmd = Batcher $ \ref -> liftIO $ do
    someCmds <- readIORef ref
    resultRef <- newIORef (error "Result of command not written back!")
    let write r = liftIO $ writeIORef resultRef r
    writeIORef ref (SomeCommand cmd write : someCmds)
    pure $ Blocked $ Batcher $ \_ref -> liftIO $ Done <$> readIORef resultRef

-- | Similar as 'schedule' but optimized for commands that have no result.
schedule_ :: (MonadIO m) => command () -> Batcher command m ()
schedule_ cmd = Batcher $ \ref -> liftIO $ do
    someCmds <- readIORef ref
    writeIORef ref (SomeCommand cmd (\_r -> pure ()) : someCmds)
    pure $ Blocked $ pure ()

{-# NOINLINE [1] schedule #-}
{-# RULES "schedule->schedule_"
          schedule = schedule_ :: (MonadIO m) => command () -> Batcher command m () #-}

-- | Execute a @Batcher@ computation using the given @Worker@.
runBatcher :: (MonadIO m) => Worker command m -> Batcher command m a -> m a
runBatcher work m = do
    ref <- liftIO $ newIORef []
    let go b = do
          rx <- unBatcher b ref
          case rx of
            Done x -> pure x
            Blocked bx -> do
              someCmds <- liftIO $ readIORef ref
              liftIO $ writeIORef ref []
              work $ reverse someCmds
              go bx
    go m

-- | A @Worker@ is responsible for executing the given batch of
-- commands. Instead of executing each command individually it might group
-- commands and execute each group in one go. It might also execute each command
-- concurrently.
--
-- The @Worker@ should ensure that the result of /each/ command is written using
-- 'writeResult'.
type Worker command m = [SomeCommand command m] -> m ()

-- | A convenience @Worker@ that simply executes commands using the given
-- function without any batching.
simpleWorker :: (Monad m) => (forall r. command r -> m r) -> Worker command m
simpleWorker exe = traverse_ $ \(SomeCommand cmd write) -> exe cmd >>= write

(<&>) :: Functor f => f a -> (a -> b) -> f b
m <&> f = f <$> m
