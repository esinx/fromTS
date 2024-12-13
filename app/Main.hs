module Main where

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as Map
import Data.Map
import FromTS (fromTS)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.Environment qualified as Env
import System.IO qualified as IO
import System.IO.Error qualified as IO
import System.Process (readProcessWithExitCode)
import TSError qualified
import TSParser (parseTSFile)

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [filename] -> do
      result <- fromTS filename
      -- ast <- parseTSFile filename
      -- print ast
      case result of
        Right js -> putStrLn js
        Left err -> print err
    _ -> putStrLn "Usage: fromTS <filename>"
