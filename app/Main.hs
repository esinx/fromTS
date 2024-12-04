module Main where

import FromTS (fromTS)
import System.Environment qualified as Env
import System.IO qualified as IO
import System.IO.Error qualified as IO
import TSError (TSError, fromTSError)

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [filename] -> do
      result <- fromTSFile filename
      case result of
        Right js -> putStrLn js
        Left err -> putStrLn $ fromTSError err
    _ -> putStrLn "Usage: fromTS <filename>"

fromTSFile :: String -> IO (Either TSError String)
fromTSFile filename = do
  handle <- IO.openFile filename IO.ReadMode
  str <- IO.hGetContents handle
  pure $ fromTS str
