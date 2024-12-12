module TSTypeChecker where

import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Control.Monad.Reader qualified as S
import Control.Monad.State
import Control.Monad.State qualified as S
import Data.Functor
import Data.List qualified as List
import Data.Map qualified as Map
import GHC.IO (unsafePerformIO)
import Safe (atMay)
import TSError
import TSNumber
import TSSyntax
import TSType
import Prelude

typeCheckConstLiteral :: Literal -> TSTypeChecker TSType
typeCheckConstLiteral (NumberLiteral (Double d)) = return $ TNumberLiteral d
typeCheckConstLiteral (NumberLiteral _) = return TNumber
typeCheckConstLiteral (StringLiteral s) = return $ TStringLiteral s
typeCheckConstLiteral (BooleanLiteral b) = return $ TBooleanLiteral b
typeCheckConstLiteral (ObjectLiteral m) = do
  m' <- traverse (typeCheckExpr' False) m
  return $ TUserObject m'
typeCheckConstLiteral NullLiteral = return TNull
typeCheckConstLiteral UndefinedLiteral = return TUndefined

typeCheckLiteral :: Literal -> TSTypeChecker TSType
typeCheckLiteral (NumberLiteral _) = return TNumber
typeCheckLiteral (StringLiteral _) = return TString
typeCheckLiteral (BooleanLiteral _) = return TBoolean
typeCheckLiteral (ObjectLiteral m) = do
  m' <- traverse (typeCheckExpr' False) m
  return $ TUserObject m'
typeCheckLiteral NullLiteral = return TNull
typeCheckLiteral UndefinedLiteral = return TUndefined

typeCheckVar :: Var -> TSTypeChecker TSType
typeCheckVar (Name n) = lookupVarType n
typeCheckVar (Dot exp n) = do
  t <- typeCheckExpr' False exp
  case t of
    TUserObject m ->
      case Map.lookup n m of
        Just t -> return t
        Nothing -> throwError $ TypeError $ "field " ++ n ++ " not found in object"
    TObject -> throwError $ TypeError $ "field " ++ n ++ " not found in object"
    _ -> throwError $ TypeError "expected object type"
typeCheckVar (Element arrExp indexExp) = do
  arr <- typeCheckExpr' False arrExp
  index <- typeCheckExpr indexExp
  case arr of
    TArray t ->
      if isSubtype index TNumber
        then return t
        else return TAny -- allowing implicit any
    TTuple ts ->
      case index of
        TNumberLiteral n -> do
          let i = floor n
          let s = length ts
          case (i >= 0 && i < s, atMay ts i) of
            (True, Just t) -> return t
            _ -> throwError $ TypeError $ "no element at index >=" ++ show s ++ " in tuple"
        t | isSubtype t TNumber -> return (TUnion ts)
        _ -> return TAny
    _ -> return TAny

typeCheckUnaryOpPrefix :: UopPrefix -> TSType -> TSTypeChecker TSType
typeCheckUnaryOpPrefix Not _ = return TBoolean
typeCheckUnaryOpPrefix BitNeg t
  | isSubtype t TNumber = return TNumber
  | otherwise = throwError $ TypeError "expected number type"
typeCheckUnaryOpPrefix TypeOf _ = return TString
typeCheckUnaryOpPrefix Spread t = return $ TArray t
typeCheckUnaryOpPrefix DecPre t
  | isSubtype t TNumber = return TNumber
  | otherwise = throwError $ TypeError "expected number type"
typeCheckUnaryOpPrefix IncPre t
  | isSubtype t TNumber = return TNumber
  | otherwise = throwError $ TypeError "expected number type"
typeCheckUnaryOpPrefix PlusUop t
  | isSubtype t TNumber = return TNumber -- TODO: check behavior, it seems like +"a" is still of number type (no error)
  | otherwise = throwError $ TypeError "expected number type"
typeCheckUnaryOpPrefix MinusUop t
  | isSubtype t TNumber = return TNumber
  | otherwise = throwError $ TypeError "expected number type"
typeCheckUnaryOpPrefix Void _ = return TUndefined

typeCheckUnaryOpPostfix :: UopPostfix -> TSType -> TSTypeChecker TSType
typeCheckUnaryOpPostfix DecPost t
  | isSubtype t TNumber = return TNumber
  | otherwise = throwError $ TypeError "expected number type"
typeCheckUnaryOpPostfix IncPost t
  | isSubtype t TNumber = return TNumber
  | otherwise = throwError $ TypeError "expected number type"

typeCheckBinaryOp :: Expression -> Bop -> Expression -> TSTypeChecker TSType
-- t1 = t2
typeCheckBinaryOp (Var v) Assign e = do
  t1 <- typeCheckVar v
  t2 <- typeCheckExpr e
  if isSubtype t2 t1 then return t2 else throwError $ TypeError "type mismatch"
-- t1 == t2
typeCheckBinaryOp e1 Eq e2 = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  -- TODO: there are some cases (e.g. 1 == {}) where 1 is technically a subtype
  -- of {}, but we should throw error in this case. however, it seems like
  -- we are not going to parse {} as type {}/ Object but instead as object literal.
  -- there are also cases like:
  --          const x = 1;
  --          const y = {};
  --          x == y; // does not error
  -- where if we parse {} as object literal instead of type {}, then it will error...
  if isSubtype t1 t2 || isSubtype t2 t1
    then return TBoolean
    else throwError $ TypeError "type mismatch"
typeCheckBinaryOp e1 PlusBop e2 = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  if isSubtype t1 TString || isSubtype t2 TString
    then return TString
    else
      if isSubtype t1 TNumber && isSubtype t2 TNumber
        then return TNumber
        else throwError $ TypeError "expected string or number type"
typeCheckBinaryOp (Var v) PlusAssign e = do
  t1 <- typeCheckVar v
  t2 <- typeCheckExpr e
  if isSubtype t1 TString || isSubtype t2 TString
    then return TString
    else
      if isSubtype t1 TNumber && isSubtype t2 TNumber
        then return TNumber
        else throwError $ TypeError "expected string or number type"
typeCheckBinaryOp e1 And e2 =
  do
    t1 <- typeCheckExpr e1
    t2 <- typeCheckExpr e2
    case (isTruthy t1, isTruthy t2) of
      (Just True, _) -> return t2
      (Just False, _) -> return t1
      _ -> return $ TUnion [t1, t2]
typeCheckBinaryOp e1 op e2
  | op `elem` [Or, NullishCoalescing] =
      do
        t1 <- typeCheckExpr e1
        t2 <- typeCheckExpr e2
        case (isTruthy t1, isTruthy t2) of
          (Just True, _) -> return t1
          (Just False, _) -> return t2
          _ -> return $ TUnion [t1, t2]
typeCheckBinaryOp e1 op e2
  | op
      `elem` [ MinusBop,
               Times,
               Div,
               Mod,
               Exp,
               BitAnd,
               BitOr,
               BitXor,
               LeftShift,
               RightShift,
               UnsignedRightShift
             ] = do
      t1 <- typeCheckExpr e1
      t2 <- typeCheckExpr e2
      if isSubtype t1 TNumber && isSubtype t2 TNumber
        then return TNumber
        else throwError $ TypeError "expected number type"
typeCheckBinaryOp (Var v) op e
  | op
      `elem` [ MinusAssign,
               TimesAssign,
               DivAssign,
               ModAssign,
               ExpAssign,
               BitAndAssign,
               BitOrAssign,
               BitXorAssign,
               LeftShiftAssign,
               RightShiftAssign,
               UnsignedRightShiftAssign
             ] = do
      t1 <- typeCheckVar v
      t2 <- typeCheckExpr e
      if isSubtype t1 TNumber && isSubtype t2 TNumber
        then return TNumber
        else throwError $ TypeError "expected number type"
typeCheckBinaryOp _ _ _ = throwError $ TypeError "unsupported binary operation"

-- | typechecks an expression, first argument indicate if we want to get the literal type
typeCheckExpr' :: Bool -> Expression -> TSTypeChecker TSType
typeCheckExpr' getLiteralType (Lit l) =
  if getLiteralType then typeCheckConstLiteral l else typeCheckLiteral l
typeCheckExpr' _ (Var v) = typeCheckVar v
typeCheckExpr' _ (AnnotatedExpression t e) = do
  e' <- typeCheckExpr e
  if isSubtype e' t then return t else throwError $ TypeError "type mismatch"
typeCheckExpr' _ (UnaryOpPrefix op e) = do
  t <- typeCheckExpr e
  typeCheckUnaryOpPrefix op t
typeCheckExpr' _ (UnaryOpPostfix e op) = do
  t <- typeCheckExpr e
  typeCheckUnaryOpPostfix op t
typeCheckExpr' _ (BinaryOp e1 op e2) = typeCheckBinaryOp e1 op e2
typeCheckExpr' _ (Array es) = do
  ts <- traverse (typeCheckExpr' False) es
  return $ TArray $ simplify (TUnion ts)
-- TODO: functions
typeCheckExpr' _ _ = undefined

-- | typechecks an expression
typeCheckExpr :: Expression -> TSTypeChecker TSType
typeCheckExpr = typeCheckExpr' True

-- | typechecks a statement
typeCheckStmt :: Statement -> Maybe TSType -> TSTypeChecker TSTypeEnv -> TSTypeChecker TSTypeEnv
typeCheckStmt (AnyExpression e) _ comp = do
  _ <- typeCheckExpr e
  comp
typeCheckStmt (ConstAssignment (Name n) e) toReturn comp = do
  t <- typeCheckExpr e
  putVarEnv n t comp
typeCheckStmt (LetAssignment (Name n) e) toReturn comp =
  do
    t <- typeCheckExpr' False e
    putVarEnv n t comp
typeCheckStmt (If condBlocks elseBlock) toReturn comp = do
  mapM_
    ( \(condBlock, block) -> do
        t <- typeCheckExpr condBlock
        if isSubtype t TBoolean || isSubtype t TNumber -- TODO: not sure if more things should be allowed
          then createNewVarEnv $ typeCheckBlock block toReturn
          else throwError $ TypeError "expected boolean type"
    )
    condBlocks
  _ <- createNewVarEnv $ typeCheckBlock elseBlock toReturn
  comp
typeCheckStmt (For varInit guard incr block) toReturn comp = do
  _ <-
    createNewVarEnv $
      typeCheckStmt
        varInit
        Nothing
        ( do
            t <- typeCheckExpr guard
            if isSubtype t TBoolean || isSubtype t TNumber -- TODO: not sure if more things should be allowed
              then do
                _ <- typeCheckExpr incr
                typeCheckBlock block toReturn
              else throwError $ TypeError "expected boolean type"
        )
  comp
typeCheckStmt (While e block) toReturn comp = do
  t <- typeCheckExpr e
  if isSubtype t TBoolean || isSubtype t TNumber -- TODO: not sure if more things should be allowed
    then do
      _ <- createNewVarEnv $ typeCheckBlock block toReturn
      comp
    else throwError $ TypeError "expected boolean type"
typeCheckStmt Break _ comp = comp
typeCheckStmt Continue _ comp = comp
typeCheckStmt (Try tryBlock (Just (Var (Name n))) catchBlock finallyBlock) toReturn comp = do
  _ <- typeCheckBlock tryBlock toReturn
  _ <- putVarEnv n TAny (createNewVarEnv $ typeCheckBlock catchBlock toReturn)
  _ <- typeCheckBlock finallyBlock toReturn
  comp
typeCheckStmt (Try tryBlock _ catchBlock _) toReturn comp =
  throwError $ TypeError "expected identifier in catch block"
typeCheckStmt (Return (Just e)) toReturn comp = do
  t <- typeCheckExpr e
  case toReturn of
    Just t' -> if isSubtype t t' then comp else throwError $ TypeError "type mismatch"
    Nothing -> throwError $ TypeError "cannot return in this context"
typeCheckStmt (Return Nothing) toReturn comp = do
  case toReturn of
    Just t' -> if isSubtype t' (TUnion [TVoid, TUndefined]) then comp else throwError $ TypeError "type mismatch"
    Nothing -> comp
-- TODO: functions
typeCheckStmt Empty _ comp = comp
typeCheckStmt _ _ _ = undefined

-- | typechecks a block
typeCheckBlock :: Block -> Maybe TSType -> TSTypeChecker TSTypeEnv
typeCheckBlock (Block []) _ = ask
typeCheckBlock (Block (s : ss)) toReturn = do
  typeCheckStmt s toReturn (typeCheckBlock (Block ss) toReturn)

-- | typechecks a program with the initial type environment
-- and empty variable bindings
typeCheckProgram ::
  Block ->
  Either Error (Map.Map String TSType)
typeCheckProgram b = do
  env <- runReaderT (typeCheckBlock b Nothing) initialTSTypeEnv
  case varEnvs env of
    [] -> throwError $ TypeError "empty env" -- TODO: returning empty instead of throwing error, but this should never happen
    currEnv : _ -> return currEnv
