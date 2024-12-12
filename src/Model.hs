module Model where

import Data.Map
import System.Process (readProcessWithExitCode)
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import TSType
import Prelude
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Bifunctor
import TSSyntax
import TSParser
import Data.List
import Data.Maybe

runModelTypeChecker :: String -> IO (Maybe (Map String TSType))
runModelTypeChecker fileName = do
  result <-
    readProcessWithExitCode
      "node"
      ["./model/dist/index.js", fileName]
      []
  case result of
    (ExitSuccess, stdout, _) ->
      let parsedJSON = JSON.decode $ BLU.fromString stdout :: Maybe (Map String String)
      in case parsedJSON of
        Nothing -> return Nothing
        Just obj ->
          let entries = toAscList obj in
          let parsed = fmap (Data.Bifunctor.second (parse typeP)) entries in
          let typeEntries = Data.Maybe.mapMaybe (\(name, t) -> case t of
                                  Right t' -> Just (name, t')
                                  Left _ -> Nothing) parsed in
          return $ Just $ fromList typeEntries
    (ExitFailure _, _, stderr) -> return Nothing