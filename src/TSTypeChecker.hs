module TSTypeChecker where

import TSError (Error)
import TSType
import TSSyntax


typeCheckExpr :: Expr -> State TSTypeEnv (Either Error TSType)
typeCheckExpr = undefined

typeCheckStmt :: Stmt -> State TSTypeEnv (Either Error ())
typeCheckStmt = undefined

typeCheckProgram :: Block -> Either Error TSGlobalEnv
typeCheckProgram = undefined
