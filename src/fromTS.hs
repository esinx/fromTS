module FromTS where

import Data.Map
import TSError (Error (SyntaxError, TypeError))
import TSParser
import TSPretty
import TSSyntax
import TSType
import TSTypeChecker (typeCheckProgram)
import Prelude

-- | Transpile a TypeScript source into JavaScript
fromTS :: String -> IO (Either Error String)
fromTS fileName = do
  result <- introspectTS fileName
  case result of
    Left err -> return $ Left err
    Right (ts, _) -> return $ Right (transpileTS ts)

introspectTS :: String -> IO (Either Error (Block, Map String TSType))
introspectTS fileName = do
  parsed <- parseTSFile fileName
  case parsed of
    Left err -> return $ Left $ SyntaxError (show err)
    Right ts -> case typeCheckProgram ts of
      Left err -> return $ Left $ TypeError (show err)
      Right typeMap -> return $ Right (ts, typeMap)
