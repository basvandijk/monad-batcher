{-# language ExistentialQuantification #-}
{-# language RankNTypes #-}

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
import Data.Foldable (traverse_)
import Data.Functor ((<$),($>))
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

infixl 4 <$>^, <$^, <*>^, <*^, *>^
infixl 1 >>=^

-- | An applicative monad that schedules commands for later more efficient execution.
newtype Batcher command m a = Batcher
    { unBatcher :: forall b
                 . IORef [Scheduled command m]
                -> OnDone m a b
                -> OnBlocked command m a b
                -> m b
    }

type OnDone            m a b =                   a -> m b
type OnBlocked command m a b = Batcher command m a -> m b

-- | Execute a @Batcher@ computation using the given @Worker@.
runBatcher
    :: forall command m a
     . (MonadIO m)
    => Worker command m    -- ^ Specfies how to batch and execute commands.
    -> Batcher command m a -- ^ The computation to run.
    -> m a
runBatcher work b = do
    ref <- liftIO $ newIORef []
    let run (Batcher bx) =
          bx ref (\ x  -> pure x)
                 (\bx' -> do
                    scheduledCmds <- liftIO $ readIORef ref <* writeIORef ref []
                    work scheduledCmds
                    run bx')
    run b
{-# INLINABLE runBatcher #-}

instance Functor (Batcher command m) where
    f `fmap` Batcher bx = Batcher $ \ref onDone onBlocked ->
        bx ref (\ x  -> onDone    (f       x ))
               (\bx' -> onBlocked (f <$>^ bx'))
    {-# INLINE fmap #-}

    y <$ Batcher bx = Batcher $ \ref onDone onBlocked ->
        bx ref (\_x  -> onDone     y         )
               (\bx' -> onBlocked (y <$^ bx'))
    {-# INLINE (<$) #-}

(<$>^) :: (a -> b) -> Batcher command m a -> Batcher command m b
f <$>^ Batcher bx = Batcher $ \ref onDone onBlocked ->
        bx ref (\ x  -> onDone    (f      x ))
               (\bx' -> onBlocked (f <$> bx'))

(<$^) :: b -> Batcher command m a -> Batcher command m b
y <$^ Batcher bx = Batcher $ \ref onDone onBlocked ->
        bx ref (\_x  -> onDone     y        )
               (\bx' -> onBlocked (y <$ bx'))

-- | Composing two 'Batcher' computations using the applicative combinators
-- ensures that commands scheduled in both computations are given to the
-- 'Worker' in one batch.
instance Applicative (Batcher command m) where
    pure x = Batcher $ \_ref onDone _onBlocked -> onDone x
    {-# INLINE pure #-}

    Batcher bf <*> Batcher bx = Batcher $ \ref onDone onBlocked ->
        bf ref (\ f  -> bx ref (\ x  -> onDone    ( f         x ))
                               (\bx' -> onBlocked ( f  <$>   bx')))
               (\bf' -> bx ref (\ x  -> onBlocked (bf' <&> ($ x)))
                               (\bx' -> onBlocked (bf' <*>^  bx')))
    {-# INLINE (<*>) #-}

    Batcher by *> Batcher bx = Batcher $ \ref onDone onBlocked ->
        by ref (\_y  -> bx ref (\ x  -> onDone                x  )
                               (\bx' -> onBlocked            bx' ))
               (\by' -> bx ref (\ x  -> onBlocked (by'  $>    x ))
                               (\bx' -> onBlocked (by'  *>^  bx')))
    {-# INLINE (*>) #-}

    Batcher bx <* Batcher by = Batcher $ \ref onDone onBlocked ->
        bx ref (\ x  -> by ref (\_y  -> onDone      x            )
                               (\by' -> onBlocked  (x  <$    by')))
               (\bx' -> by ref (\_y  -> onBlocked  bx'           )
                               (\by' -> onBlocked (bx' <*^   by')))
    {-# INLINE (<*) #-}

(<*>^) :: Batcher command m (a -> b) -> Batcher command m a -> Batcher command m b
Batcher bf <*>^ Batcher bx = Batcher $ \ref onDone onBlocked ->
        bf ref (\ f  -> bx ref (\ x  -> onDone    ( f         x ))
                               (\bx' -> onBlocked ( f  <$>   bx')))
               (\bf' -> bx ref (\ x  -> onBlocked (bf' <&> ($ x)))
                               (\bx' -> onBlocked (bf' <*>   bx')))

(*>^) :: Batcher command m b -> Batcher command m a -> Batcher command m a
Batcher by *>^ Batcher bx = Batcher $ \ref onDone onBlocked ->
        by ref (\_y  -> bx ref (\ x  -> onDone                x  )
                               (\bx' -> onBlocked            bx' ))
               (\by' -> bx ref (\ x  -> onBlocked (by'  $>    x ))
                               (\bx' -> onBlocked (by'  *>   bx')))

(<*^) :: Batcher command m b -> Batcher command m a -> Batcher command m b
Batcher bx <*^ Batcher by = Batcher $ \ref onDone onBlocked ->
        bx ref (\ x  -> by ref (\_y  -> onDone      x            )
                               (\by' -> onBlocked  (x  <$    by')))
               (\bx' -> by ref (\_y  -> onBlocked  bx'           )
                               (\by' -> onBlocked (bx' <*^   by')))

instance Monad (Batcher command m) where
    return = pure

    Batcher bx >>= f = Batcher $ \ref onDone onBlocked ->
        bx ref (\ x  -> unBatcher (f x) ref onDone onBlocked)
               (\bx' -> onBlocked (bx' >>=^ f))
    {-# INLINE (>>=) #-}

    (>>) = (*>)

(>>=^)
    :: Batcher command m a
    -> (a -> Batcher command m b)
    -> Batcher command m b
Batcher bx >>=^ f = Batcher $ \ref onDone onBlocked ->
        bx ref (\ x  -> unBatcher (f x) ref onDone onBlocked)
               (\bx' -> onBlocked (bx' >>= f))

-- | Catch and handle exceptions.
catchBatcher
    :: (MonadCatch m, Exception e)
    => Batcher command m a        -- ^ The computation to run
    -> (e -> Batcher command m a) -- ^ Handler to invoke if an exception is raised
    -> Batcher command m a
Batcher bx `catchBatcher` h = Batcher $ \ref onDone onBlocked -> do
    e <- try $ bx ref (\ x  -> pure (Done     x ))
                      (\bx' -> pure (Blocked bx'))
    case e of
      Left ex -> unBatcher (h ex) ref onDone onBlocked
      Right r ->
        case r of
          Done     x  -> onDone x
          Blocked bx' -> onBlocked (bx' `catchBatcher` h)
{-# INLINEABLE catchBatcher #-}

data Result command m a = Done a | Blocked (Batcher command m a)

-- | Schedule a command for later execution.
schedule
    :: (MonadIO m, MonadThrow m)
    => command a -- ^ The command to schedule.
    -> Batcher command m a
schedule cmd = Batcher $ \ref _onDone onBlocked ->
    registerCommand ref cmd >>= onBlocked . getResult
{-# INLINABLE schedule #-}

registerCommand
    :: (MonadIO m)
    => IORef [Scheduled command m]
    -> command a
    -> m (MVar (Either SomeException a))
registerCommand ref cmd = liftIO $ do
    scheduledCmds <- readIORef ref
    resultMVar <- newEmptyMVar
    writeIORef ref (Scheduled cmd (liftIO . putMVar resultMVar) : scheduledCmds)
    pure resultMVar

getResult
    :: (MonadIO m, MonadThrow m)
    => MVar (Either SomeException a)
    -> Batcher command m a
getResult resultMVar = Batcher $ \_ref onDone _onBlocked -> do
    r <- liftIO $ takeMVar resultMVar
    case r of
      Left ex -> throwM ex
      Right x -> onDone x

-- | A @Worker@ is responsible for executing the given batch of scheduled
-- commands.
--
-- Note that the list of scheduled commands is given in LIFO (Last In First Out)
-- order. In other words the last scheduled command is the head of the list. So
-- do reverse the list if you depend on executing commands in the order they
-- were given.
--
-- Instead of executing each command individually a @Worker@ might group
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

-- | A convenience @Worker@ that simply executes the scheduled
-- commands in the order in which they are specified using the given
-- function without any batching.
simpleWorker
    :: (MonadCatch m)
    => (forall r. command r -> m r) -- ^ Specifies how to execute a command.
    -> Worker command m
simpleWorker exe = traverse_ (\(Scheduled cmd write) -> try (exe cmd) >>= write) . reverse
{-# INLINEABLE simpleWorker #-}

-- | Infix flipped 'fmap'.
(<&>) :: Functor f => f a -> (a -> b) -> f b
m <&> f = f <$> m
{-# INLINE (<&>) #-}
infixl 1 <&>
