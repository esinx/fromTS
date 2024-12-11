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
  -- res <- parseTSFile "./playground/typescript-ast-parser/literals-simple.ts"
  res <- parseTSFile "./playground/typescript-ast-parser/literals-simple.ts"
  case res of
    Right ts -> print ts
    Left err -> print err

-- main :: IO ()
-- main = do
--   args <- Env.getArgs
--   case args of
--     [filename] -> do
--       result <- fromTSFile filename
--       case result of
--         Right js -> print js
--         Left err -> print err
--     _ -> putStrLn "Usage: fromTS <filename>"

-- fromTSFile :: String -> IO (Either TSError.Error String)
-- fromTSFile filename = do
--   handle <- IO.openFile filename IO.ReadMode
--   str <- IO.hGetContents handle
--   pure $ fromTS str
