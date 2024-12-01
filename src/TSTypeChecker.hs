module TSTypeChecker where

import Control.Monad.Reader
import Control.Monad.Reader qualified as S
import Control.Monad.State
import Control.Monad.State qualified as S
import Data.Functor
import TSError
import TSSyntax
import TSType

-- | checks if a type is a subtype of another type
isSubtype :: TSType -> TSType -> Bool
isSubtype t1 t2 = t1 == t2

-- | typechecks an expression
typeCheckExpr :: Expression -> TSTypeChecker TSType
typeCheckExpr (Lit (BooleanLiteral _)) = return TBoolean
typeCheckExpr (Var (Name n)) = lookupVarType n
typeCheckExpr _ = undefined

-- | typechecks a statement
typeCheckStmt :: Statement -> TSTypeChecker ()
typeCheckStmt = undefined

-- | typechecks a block
typeCheckBlock :: Block -> TSTypeChecker ()
typeCheckBlock (Block []) = return ()
typeCheckBlock (Block (s : ss)) = do
  typeCheckStmt s
  typeCheckBlock (Block ss)

-- | typechecks a program with the initial type environment
-- and empty variable bindings
typeCheckProgram ::
  Block ->
  Either Error TSGlobalEnv
typeCheckProgram b = do
  runReaderT (typeCheckBlock b >> asks globalEnv) initialTSTypeEnv
