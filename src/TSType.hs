module TSType where

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State qualified as S
import Data.Map (Map)
import Data.Map qualified as Map
import TSError

type TSGlobalEnv = Map String TSType

type TSLocalEnv = Map String TSType

type TSObjectEnv = Map String TSType

data TSTypeEnv = TSTypeEnv
  { globalEnv :: TSGlobalEnv,
    localEnv :: TSLocalEnv,
    objectEnv :: TSObjectEnv
  }
  deriving (Show, Eq)

data TSType
  = TBoolean -- boolean
  | TBooleanLiteral Bool -- true, false
  | TNumber -- number
  | TNumberLiteral Double -- 1, 2.3
  | TString -- string
  | TStringLiteral String -- "hello"
  | TArray TSType -- Array<T>
  | TTuple TSType TSType -- [T, U]
  | TEnum (Map String TSType)
  | TBracket -- {}
  | TObject
  | TUserObject (Map String TSType)
  | TFunction [TSType] TSType
  | TUnknown -- proper top
  | TAny -- chaotic top type
  | TNever -- bottom type
  | TVoid
  | TNull
  | TUndefined
  | TUnion [TSType]
  | TIntersection [TSType]
  deriving (Show, Eq)

type TSTypeChecker = ReaderT TSTypeEnv (Either Error)

initialTSTypeEnv :: TSTypeEnv
initialTSTypeEnv =
  TSTypeEnv
    { globalEnv = Map.empty,
      localEnv = Map.empty,
      objectEnv = Map.empty
    }

updateGlobalEnv :: String -> TSType -> TSTypeChecker ()
updateGlobalEnv name t = do
  env <- ask
  local (\env -> env {objectEnv = Map.insert name t (globalEnv env)}) (return ())

updateLocalEnv :: String -> TSType -> TSTypeChecker ()
updateLocalEnv name t = do
  env <- ask
  local (\env -> env {objectEnv = Map.insert name t (localEnv env)}) (return ())

updateObjectEnv :: String -> TSType -> TSTypeChecker ()
updateObjectEnv name t = do
  env <- ask
  local (\env -> env {objectEnv = Map.insert name t (objectEnv env)}) (return ())

lookupVarType :: String -> TSTypeChecker TSType
lookupVarType name = do
  env <- ask
  case Map.lookup name (localEnv env) <|> Map.lookup name (globalEnv env) of
    Just t -> return t
    Nothing -> throwError $ TypeError $ "Variable " ++ name ++ " not found in the environment"

lookupObjectType :: String -> TSTypeChecker TSType
lookupObjectType name = do
  env <- ask
  case Map.lookup name (objectEnv env) of
    Just t -> return t
    Nothing -> throwError $ TypeError $ "Object " ++ name ++ " not found in the environment"