{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language ApplicativeDo #-}

module Main (main) where

import Control.Monad.Batcher
import Control.Exception (SomeException, try)
import Data.Foldable
import Data.List
import Prelude hiding (putStrLn, getLine)
import qualified System.IO (putStrLn, getLine)

main :: IO ()
main = runBatcher batchedWorker batcher

batcher :: Batcher Command IO ()
batcher = do
    putStrLn "Welcome to the example of Control.Monad.Batcher!"
    putStrLn "Please type your name "
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

data Command r where
    PutStrLn :: String -> Command ()
    GetLine  :: Command String

batchedWorker :: Worker Command IO
batchedWorker cmds = for_ (batch cmds) $ \b -> do
                       System.IO.putStrLn "BATCH:"
                       case b of
                         OpPutStrLns strs writes -> do
                             r <- try $ System.IO.putStrLn $ intercalate "\n" $ reverse strs
                             for_ writes $ \write -> write r
                         OpGetLine write -> do
                             r <- try $ System.IO.getLine
                             write r

data Batch = OpPutStrLns [String] [Either SomeException () -> IO ()]
           | OpGetLine (Either SomeException String -> IO ())

batch :: [Scheduled Command IO] -> [Batch]
batch [] = []
batch (Scheduled (PutStrLn str) write : cmds) = batchPutStrLns [str] [write] cmds
batch (Scheduled GetLine write        : cmds) = OpGetLine write : batch cmds

batchPutStrLns :: [String] -> [Either SomeException () -> IO ()] -> [Scheduled Command IO] -> [Batch]
batchPutStrLns strs writes []                                      = OpPutStrLns strs writes : []
batchPutStrLns strs writes cmds@(Scheduled GetLine _       : _)    = OpPutStrLns strs writes : batch cmds
batchPutStrLns strs writes (Scheduled (PutStrLn str) write : cmds) = batchPutStrLns (str:strs) (write:writes) cmds
