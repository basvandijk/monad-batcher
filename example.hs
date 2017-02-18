{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language ApplicativeDo #-}

module Main (main) where

import Control.Monad.Batcher
import Data.List.NonEmpty ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Foldable
import Data.List
import Data.Maybe
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
    pure () -- TODO: Please notify GHCHQ about ApplicativeDo's failure to put
            -- the two preceding lines in Applicative style when the final
             -- `pure ()` is absent.

putStrLn :: String -> Batcher Command IO ()
putStrLn str = schedule_ (PutStrLn str)

getLine :: Batcher Command IO String
getLine = schedule GetLine

data Command r where
    PutStrLn :: String -> Command ()
    GetLine  :: Command String

unbatchedWorker :: Worker Command IO
unbatchedWorker = simpleWorker $ \case
                    PutStrLn str -> System.IO.putStrLn str
                    GetLine      -> System.IO.getLine

batchedWorker :: Worker Command IO
batchedWorker cmds = traverse_ exeBatch batches
  where
    batches :: [NonEmpty (SomeCommand Command IO)]
    batches = NonEmpty.groupBy inBatch cmds
      where
        inBatch :: SomeCommand Command IO -> SomeCommand Command IO -> Bool
        inBatch (SomeCommand PutStrLn{} _) (SomeCommand PutStrLn{} _) = True
        inBatch (SomeCommand GetLine{}  _) (SomeCommand GetLine{}  _) = True
        inBatch _                          _                          = False

    exeBatch :: NonEmpty (SomeCommand Command IO) -> IO ()
    exeBatch (someCmd@(SomeCommand cmd _) :| someCmds) = do
        System.IO.putStrLn "BATCH:"
        case cmd of
          PutStrLn{} -> System.IO.putStrLn $ intercalate "\n" $ getStrings (someCmd:someCmds)
          GetLine{}  -> unbatchedWorker (someCmd:someCmds)

    getStrings :: [SomeCommand Command IO] -> [String]
    getStrings = mapMaybe mbGetString
        where
          mbGetString :: SomeCommand Command IO -> Maybe String
          mbGetString (SomeCommand (PutStrLn str) _) = Just str
          mbGetString (SomeCommand GetLine{}      _) = Nothing
