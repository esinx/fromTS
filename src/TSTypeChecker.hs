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
import TSError
import TSSyntax
import TSType

-- | for equivalence
simplify :: TSType -> TSType
-- {} | null | undefined = unknown
simplify (TUnion ts) | List.sort ts == List.sort [TBracket, TNull, TUndefined] = TUnknown
simplify (TIntersection ts) | TNever `elem` ts = TNever
simplify (TUnion ts) = simplifyUnions (TUnion ts) False
  where
    -- TODO: there might be other types that are equivalent to some union types
    simplifyUnions (TUnion ts) False = simplifyUnions (TUnion $ List.sort $ List.nub $ map simplify ts) True
    simplifyUnions (TUnion [t]) _ = simplify t
    simplifyUnions t _ = t
simplify (TIntersection ts) = simplifyIntersections (TIntersection ts) False
  where
    simplifyIntersections (TIntersection ts) False = simplifyIntersections (TIntersection $ List.sort $ List.nub $ map simplify ts) True
    simplifyIntersections (TIntersection [t]) _ = simplify t
    simplifyIntersections t _ = t
simplify t = t

isSubtype :: TSType -> TSType -> Bool
isSubtype t1 t2 = isSubtype' (simplify t1) (simplify t2)

-- | checks if a type is a subtype of another type
isSubtype' :: TSType -> TSType -> Bool
-- reflexivity
isSubtype' t1 t2 | t1 == t2 = True
-- union
isSubtype' (TUnion ts) t = all (`isSubtype'` t) ts
isSubtype' t (TUnion ts) = any (isSubtype' t) ts
-- intersection
isSubtype' (TIntersection ts) t = any (`isSubtype'` t) ts
isSubtype' t (TIntersection ts) = all (isSubtype' t) ts
-- proper bottom type
isSubtype' TNever _ = True
-- proper top type
isSubtype' _ TUnknown = True
-- chaotic top/ bottom type
isSubtype' _ TAny = True
isSubtype' TAny TNever = False
isSubtype' TAny _ = True
-- nothing is a subtype of null except bottom types
isSubtype' _ TNull = False
-- nothing is a subtype of undefined except bottom types
isSubtype' _ TUndefined = False
-- nothing is a subtype of void except bottom types and undefined
isSubtype' TUndefined TVoid = True
isSubtype' _ TVoid = False
-- nothing is a subtype of literal types except bottom types
isSubtype' _ (TStringLiteral _) = False
isSubtype' _ (TNumberLiteral _) = False
isSubtype' _ (TBooleanLiteral _) = False
-- T1 <: S1       S2 <: T2
-- ----------------------
--    S1 → S2 <: T1 → T2
isSubtype' (TFunction args1 ret1) (TFunction args2 ret2) =
  length args1 <= length args2 -- contravariant
    && all (uncurry isSubtype') (zip args2 args1)
    && isSubtype' ret1 ret2
-- nothing is a subtype of functions except bottom types
isSubtype' _ (TFunction _ _) = False
-- S1 <: T1       S2 <: T2
-- ------------------------
--    S1 * S2 <: T1 * T2
isSubtype' (TTuple t1 t2) (TTuple u1 u2) = isSubtype' t1 u1 && isSubtype' t2 u2
-- nothing is a subtype of tuples except bottom types
isSubtype' _ (TTuple _ _) = False
-- nothing is a subtype of arrays except bottom types and tuples
isSubtype' (TArray t1) (TArray t2) = isSubtype' t1 t2
isSubtype' (TTuple t1 t2) (TArray arrType) = isSubtype' (TUnion [t1, t2]) arrType
isSubtype' _ (TArray _) = False
--              n > m
-- ---------------------------------- (Width Subtyping)
-- {i1:T1...in:Tn} <: {i1:T1...im:Tm}
--      S1 <: T1  ...  Sn <: Tn
-- ---------------------------------- (Depth Subtyping)
-- {i1:S1...in:Sn} <: {i1:T1...in:Tn}
-- object 1's fields is a permutation of fields in object 2
isSubtype' (TUserObject n) (TUserObject m) =
  length n >= length m
    && all
      ( \(mKey, mType) ->
          case Map.lookup mKey n of
            Just nType -> isSubtype' nType mType
            Nothing -> False
      )
      (Map.toList m)
-- nothing is a subtype of user objects except bottom types
isSubtype' _ (TUserObject _) = False
-- bottom types, function, tuple, array, user objects are subtypes of object
isSubtype' (TFunction _ _) TObject = True
isSubtype' (TTuple _ _) TObject = True
isSubtype' (TArray _) TObject = True
isSubtype' (TUserObject _) TObject = True
isSubtype' _ TObject = False
-- bottom types, string literal are subtypes of string
isSubtype' (TStringLiteral _) TString = True
isSubtype' _ TString = False
-- bottom types, number literal are subtypes of number
isSubtype' (TNumberLiteral _) TNumber = True
isSubtype' _ TNumber = False
-- bottom types, boolean literal are subtypes of boolean
isSubtype' (TBooleanLiteral _) TBoolean = True
isSubtype' _ TBoolean = False
-- everything except null, void, undefined, unknown are subtypes of bracket
isSubtype' t TBracket
  | t == TNull || t == TVoid || t == TUndefined || t == TUnknown = False
  | otherwise = True
isSubtype' _ _ = False

typeCheckConstLiteral :: Literal -> TSTypeChecker TSType
typeCheckConstLiteral (IntegerLiteral n) = return $ TNumberLiteral n
typeCheckConstLiteral (StringLiteral s) = return $ TStringLiteral s
typeCheckConstLiteral (BooleanLiteral b) = return $ TBooleanLiteral b
typeCheckConstLiteral (ObjectLiteral m) = do
  m' <- traverse (typeCheckExpr' False) m
  return $ TUserObject m'
typeCheckConstLiteral NullLiteral = return TNull
typeCheckConstLiteral UndefinedLiteral = return TUndefined

typeCheckLiteral :: Literal -> TSTypeChecker TSType
typeCheckLiteral (IntegerLiteral _) = return TNumber
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
    TTuple t u ->
      case index of
        TNumberLiteral 0 -> return t
        TNumberLiteral 1 -> return u
        TNumberLiteral _ -> throwError $ TypeError "no element at index >=2 in tuple"
        t | isSubtype t TNumber -> return (TUnion [t, u])
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
typeCheckBinaryOp _ _ _ = undefined

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
typeCheckStmt (ConstAssignment (Name n) e) toReturn comp = do
  t <- typeCheckExpr e
  putVarEnv n t comp
typeCheckStmt (LetAssignment (Name n) e) toReturn comp =
  do
    t <- typeCheckExpr' False e
    putVarEnv n t comp
typeCheckStmt (If e thenBlock elseBlock) toReturn comp = do
  t <- typeCheckExpr e
  if isSubtype t TBoolean || isSubtype t TNumber -- TODO: not sure if more things should be allowed
    then do
      _ <- createNewVarEnv $ typeCheckBlock thenBlock toReturn
      _ <- createNewVarEnv $ typeCheckBlock elseBlock toReturn
      comp
    else throwError $ TypeError "expected boolean type"
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
typeCheckStmt (Try tryBlock (Var (Name n)) catchBlock) toReturn comp = do
  _ <- typeCheckBlock tryBlock toReturn
  _ <- putVarEnv n TAny (createNewVarEnv $ typeCheckBlock catchBlock toReturn)
  comp
typeCheckStmt (Try tryBlock _ catchBlock) toReturn comp =
  throwError $ TypeError "expected identifier in catch block"
typeCheckStmt (Return e) toReturn comp = do
  t <- typeCheckExpr e
  case toReturn of
    Just t' -> if isSubtype t t' then comp else throwError $ TypeError "type mismatch"
    Nothing -> throwError $ TypeError "cannot return in this context"
-- TODO: typeCheckStmt (Switch e cases) toReturn comp =, functions
typeCheckStmt (LabeledStatement _ s) toReturn comp = typeCheckStmt s toReturn comp
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
