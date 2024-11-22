module TSTypeChecker where

import Control.Monad.State
import Control.Monad.State qualified as S
import Data.Functor
import TSError (Error)
import TSSyntax
import TSType

-- Q: Can state handle the context for diverging branches e.g. if-then-else?

-- | checks if a type is a subtype of another type
isSubtype :: TSType -> TSType -> Bool
isSubtype t1 t2 = t1 == t2

-- | typechecks an expression
typeCheckExpr :: Expression -> State TSTypeEnv (Either Error TSType)
typeCheckExpr (Lit (BooleanLiteral _)) = return $ Right TBoolean
typeCheckExpr _ = undefined

-- | typechecks a statement
typeCheckStmt :: Statement -> State TSTypeEnv (Either Error ())
typeCheckStmt = undefined

-- | typechecks a block
typeCheckBlock :: Block -> State TSTypeEnv (Either Error ())
typeCheckBlock (Block []) = pure $ Right ()
typeCheckBlock (Block (s : ss)) = do
  r <- typeCheckStmt s
  case r of
    Left e -> return $ Left e
    Right _ -> typeCheckBlock (Block ss)

-- | typechecks a program with the initial type environment
-- and empty variable bindings
typeCheckProgram :: Block -> Either Error TSGlobalEnv
typeCheckProgram b = do
  let (r, env) =
        S.runState (typeCheckBlock b) initialTSTypeEnv
  case r of
    Left e -> Left e
    Right _ -> Right $ globalEnv env
