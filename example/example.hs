{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language ApplicativeDo #-}
{-# language ExistentialQuantification #-}

module Main (main) where

import Control.Monad.Batcher
import Control.Exception (Exception, toException, SomeException, try)
import Data.Foldable
import Data.List
import Data.Typeable
import Prelude hiding (putStrLn, getLine)
import qualified System.IO (putStrLn, getLine)

main :: IO ()
main = runBatcher unbatchedWorker batcher

batcher :: Batcher Command IO ()
batcher = do
    putStrLn "Welcome to the example of Control.Monad.Batcher!"
    putStrLn "Please type your name "
    _ <- throw MyException `catchBatcher` \MyException -> putStrLn "exception!"
    putStrLn "and age."
    name <- getLine
    age <- getLine
    putStrLn $ "Hello " ++ name
    putStrLn $ age ++ " is not that old!"
    pure ()

putStrLn :: String -> Batcher Command IO ()
putStrLn str = schedule (PutStrLn str)

getLine :: Batcher Command IO String
getLine = schedule GetLine

throw :: (Exception e) => e -> Batcher Command IO a
throw e = schedule (Throw e)

data MyException = MyException deriving (Typeable, Show)

instance Exception MyException

data Command r where
    PutStrLn :: String -> Command ()
    GetLine  :: Command String
    Throw    :: (Exception e) => e -> Command a

unbatchedWorker :: Worker Command IO
unbatchedWorker [] = pure ()
unbatchedWorker (Scheduled (PutStrLn str) write : cmds) = do r <- try (System.IO.putStrLn str)
                                                             write r
                                                             unbatchedWorker cmds
unbatchedWorker (Scheduled GetLine        write : cmds) = do r <- try System.IO.getLine
                                                             write r
                                                             unbatchedWorker cmds
unbatchedWorker (Scheduled (Throw e)      write : cmds) = do write (Left $ toException e)
                                                             -- continue cmds
                                                             unbatchedWorker cmds -- !!!

_unused :: Worker Command IO
_unused = batchedWorker

batchedWorker :: Worker Command IO
batchedWorker cmds = do
    System.IO.putStrLn "WORKER:"
    for_ (batch cmds) $ \b -> do
      System.IO.putStrLn "BATCH:"
      case b of
        OpPutStrLns strs writes -> do
            r <- try $ System.IO.putStrLn $ intercalate "\n" $ reverse strs
            for_ writes $ \write -> write r
        OpGetLine write -> do
            r <- try $ System.IO.getLine
            write r
        OpThrow e write -> do
            write $ Left $ toException e

data Batch = OpPutStrLns [String] [Either SomeException () -> IO ()]
           | OpGetLine (Either SomeException String -> IO ())
           | forall e a. (Exception e) => OpThrow e (Either SomeException a -> IO ())

batch :: [Scheduled Command IO] -> [Batch]
batch [] = []
batch (Scheduled (PutStrLn str) write : cmds) = batchPutStrLns [str] [write] cmds
batch (Scheduled GetLine        write : cmds) = OpGetLine write : batch cmds
batch (Scheduled (Throw e)      write : cmds) = OpThrow e write : batch cmds

batchPutStrLns :: [String] -> [Either SomeException () -> IO ()] -> [Scheduled Command IO] -> [Batch]
batchPutStrLns strs writes []                                      = OpPutStrLns strs writes : []
batchPutStrLns strs writes cmds@(Scheduled GetLine   _     : _)    = OpPutStrLns strs writes : batch cmds
batchPutStrLns strs writes cmds@(Scheduled Throw{}   _     : _)    = OpPutStrLns strs writes : batch cmds
batchPutStrLns strs writes (Scheduled (PutStrLn str) write : cmds) = batchPutStrLns (str:strs) (write:writes) cmds
