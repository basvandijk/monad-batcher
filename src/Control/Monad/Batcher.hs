{-# language ExistentialQuantification #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}

module Control.Monad.Batcher
  ( Batcher

  , runBatcher
  , Worker
  , Scheduled(..)
  , simpleWorker

  , schedule
  , catchBatcher
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow, throwM, MonadCatch, Exception, SomeException, try)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Foldable (traverse_)
import Data.Functor ((<$),($>))
import Data.Semigroup (Semigroup, (<>))

-- | An applicative monad that schedules commands for later more efficient execution.
newtype Batcher batch command m a = Batcher
    { unBatcher :: forall b
                 . (a -> m b)
                -> (batch (Scheduled command) -> Batcher batch command m a -> m b)
                -> m b
    }

-- | Execute a @Batcher@ computation using the given @Worker@.
runBatcher
    :: (Applicative m)
    => Worker  batch command m   -- ^ Specfies how to batch and execute commands.
    -> Batcher batch command m a -- ^ The computation to run.
    -> m a
runBatcher work = run
  where
    run (Batcher b) = b (\x -> pure x)
                        (\batch b' -> work batch *> run b')

instance Functor (Batcher batch command m) where
    f `fmap` Batcher bx = Batcher $ \done execute ->
        bx (\       x  -> done          (f      x))
           (\batch bx' -> execute batch (f <$> bx'))

    y <$ Batcher bx = Batcher $ \done execute ->
        bx (\      _x  -> done           y)
           (\batch bx' -> execute batch (y <$  bx'))

-- | Composing two 'Batcher' computations using the applicative combinators
-- ensures that commands scheduled in both computations are given to the
-- 'Worker' in one batch.
instance (Semigroup (batch (Scheduled command))) => Applicative (Batcher batch command m) where
    pure x = Batcher $ \done _execute -> done x

    Batcher bf <*> Batcher bx = Batcher $ \done execute ->
        bf (\        f  -> bx (\        x  -> done                       ( f         x ))
                              (\batch  bx' -> execute batch              ( f  <$>   bx')))
           (\batchF bf' -> bx (\        x  -> execute batchF             (bf' <&> ($ x)))
                              (\batchX bx' -> execute (batchF <> batchX) (bf' <*>   bx')))

    Batcher by *> Batcher bx = Batcher $ \done execute ->
        by (\       _y  -> bx (\        x  -> done                         x )
                              (\batch  bx' -> execute batch               bx'))
           (\batchY by' -> bx (\        x  -> execute batchY             (by'  $>    x ))
                              (\batchX bx' -> execute (batchY <> batchX) (by'  *>   bx')))

    Batcher bx <* Batcher by = Batcher $ \done execute ->
        bx (\        x  -> by (\       _y  -> done                         x            )
                              (\batch  by' -> execute batch              ( x  <$    by')))
           (\batchX bx' -> by (\       _y  -> execute batchX              bx'           )
                              (\batchY by' -> execute (batchX <> batchY) (bx' <*    by')))

instance (Semigroup (batch (Scheduled command))) => Monad (Batcher batch command m) where
    return = pure

    Batcher b >>= f = Batcher $ \done execute ->
        b (\x -> unBatcher (f x) done execute)
          (\batch b' -> execute batch (b' >>= f))

    (>>) = (*>)

-- | Catch and handle exceptions.
catchBatcher
    :: (MonadCatch m, Exception e)
    => Batcher batch command m a        -- ^ The computation to run
    -> (e -> Batcher batch command m a) -- ^ Handler to invoke if an exception is raised
    -> Batcher batch command m a
Batcher b `catchBatcher` h = Batcher $ \done execute -> do
    e <- try $ b (\      x  -> pure (Done          x ))
                 (\batch b' -> pure (Execute batch b'))
    case e of
      Left ex -> unBatcher (h ex) done execute
      Right r ->
        case r of
          Done          x  -> done x
          Execute batch b' -> execute batch (b' `catchBatcher` h)

data Result batch command m a =
    Done a | Execute (batch (Scheduled command)) (Batcher batch command m a)

-- | Schedule a command for later execution.
schedule
    :: (MonadIO m, MonadThrow m, Applicative batch)
    => command a -- ^ The command to schedule.
    -> Batcher batch command m a
schedule cmd = Batcher $ \_done execute -> do
    resultMVar <- liftIO newEmptyMVar
    let putRes r = putMVar resultMVar r
    execute (pure (Scheduled cmd putRes)) $ Batcher $ \done _execute -> do
      r <- liftIO $ takeMVar resultMVar
      case r of
        Left ex -> throwM ex
        Right x -> done x

-- | A @Worker@ is responsible for executing the given batch of scheduled
-- commands.
--
-- Instead of executing each command individually a @Worker@ might group
-- commands and execute each group in one go. It might also execute each command
-- concurrently.
--
-- The @Worker@ should ensure that the result of /each/ command is written using
-- 'writeResult'.
type Worker batch command m = batch (Scheduled command) -> m ()

-- | A @'schedule'd@ command paired with a function to communicate its result.
--
-- Note that the result of the command is
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#ghc-flag--XExistentialQuantification existentially quantified>.
-- This ensures that multiple commands with different
-- result types can be batched in a list which can then be given to a 'Worker' for execution.
data Scheduled command = forall a.
     Scheduled
     { command   :: command a
     , putResult :: Either SomeException a -> IO ()
     }

-- | A convenience @Worker@ that simply executes the scheduled
-- commands in the order in which they are specified using the given
-- function without any batching.
simpleWorker
    :: (MonadCatch m, MonadIO m, Foldable batch)
    => (forall r. command r -> m r) -- ^ Specifies how to execute a command.
    -> Worker batch command m
simpleWorker exe = traverse_ $ \(Scheduled cmd put) -> try (exe cmd) >>= liftIO . put

-- | Infix flipped 'fmap'.
(<&>) :: Functor f => f a -> (a -> b) -> f b
m <&> f = f <$> m
{-# INLINE (<&>) #-}
infixl 1 <&>
