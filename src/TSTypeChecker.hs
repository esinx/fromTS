module TSTypeChecker where

import TSError (Error)
import TSSyntax
import Data.Map (Map)
import Data.Map qualified as Map

type TSGlobalEnv = Map String TSType
type TSLocalEnv = Map String TSType
type TSObjectEnv = Map String TSType

data TSTypeEnv = TSTypeEnv {
  globalEnv :: TSGlobalEnv,
  localEnv :: TSLocalEnv,
  objectEnv :: TSObjectEnv
}

data TSType
  = TBoolean -- boolean
  | TNumber -- number
  | TString -- string
  | TArray TSType -- Array<T>
  | TTuple TSType TSType -- [T, U]
  | TEnum [(String, TSType)]
  | TUnknown
  | TAny
  | TVoid
  | TNull
  | TUndefined
  | TNever
  | TObject (Map String TSType)
  | TFunction [TSType] TSType

typeCheckExpr :: Expr -> State TSTypeEnv (Either Error TSType)
typeCheckExpr = undefined

typeCheckStmt :: Stmt -> State TSTypeEnv (Either Error ())
typeCheckStmt = undefined

typeCheckProgram :: Block -> Either Error (Map String TSType)
typeCheckProgram = undefined
