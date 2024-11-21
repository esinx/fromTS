module TSTypeChecker where

import TSError (Error)
import TSType
import TSSyntax

import Control.Monad.State.Lazy

typeCheckExpr :: Expression -> State TSTypeEnv (Either Error TSType)
typeCheckExpr = undefined

typeCheckStmt :: Statement -> State TSTypeEnv (Either Error ())
typeCheckStmt = undefined

typeCheckProgram :: Block -> Either Error TSGlobalEnv
typeCheckProgram = undefined
