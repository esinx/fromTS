module TSType where

import Control.Applicative ((<|>))
import Control.Monad.State
import Control.Monad.State qualified as S
import Data.Map (Map)
import Data.Map qualified as Map

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
  | TNumber -- number
  | TString -- string
  | TArray TSType -- Array<T>
  | TTuple TSType TSType -- [T, U]
  | TEnum (Map String TSType)
  | TObject (Map String TSType) -- alias, interface, class
  | TFunction [TSType] TSType
  | TUnknown
  | TAny
  | TVoid
  | TNull
  | TUndefined
  | TNever
  | TUnion [TSType]
  | TIntersection [TSType]
  deriving (Show, Eq)

initialTSTypeEnv :: TSTypeEnv
initialTSTypeEnv =
  TSTypeEnv
    { globalEnv = Map.empty,
      localEnv = Map.empty,
      objectEnv =
        Map.fromList
          [ ("object", TObject Map.empty)
          ]
    }

updateGlobalEnv :: String -> TSType -> State TSTypeEnv ()
updateGlobalEnv name t = do
  env <- get
  put $ env {globalEnv = Map.insert name t (globalEnv env)}

updateLocalEnv :: String -> TSType -> State TSTypeEnv ()
updateLocalEnv name t = do
  env <- get
  put $ env {localEnv = Map.insert name t (localEnv env)}

updateObjectEnv :: String -> TSType -> State TSTypeEnv ()
updateObjectEnv name t = do
  env <- get
  put $ env {objectEnv = Map.insert name t (objectEnv env)}

lookupVarType :: String -> State TSTypeEnv (Maybe TSType)
lookupVarType name = do
  env <- get
  return $ Map.lookup name (localEnv env) <|> Map.lookup name (globalEnv env)

lookupObjectType :: String -> State TSTypeEnv (Maybe TSType)
lookupObjectType name = gets (Map.lookup name . objectEnv)