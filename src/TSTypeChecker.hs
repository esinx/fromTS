module TSTypeChecker where

import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Control.Monad.Reader qualified as S
import Control.Monad.State
import Control.Monad.State qualified as S
import Data.Functor
import Data.List qualified as List
import Data.Map qualified as Map
import TSError
import TSSyntax
import TSType

-- | for equivalence
simplify :: TSType -> TSType
-- {} | null | undefined = unknown
simplify (TUnion ts) | List.sort ts == List.sort [TBracket, TNull, TUndefined] = TUnknown
simplify (TUnion [t]) = t
simplify (TUnion ts) = TUnion $ List.sort $ List.nub $ map simplify ts
simplify (TIntersection [t]) = t
simplify (TIntersection ts) = TIntersection $ List.sort $ List.nub $ map simplify ts
-- TODO: there might be other types that are equivalent to some union types
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
        else return TAny -- TODO: check, not sure, gives error sometimes
    TTuple t u ->
      case index of
        TNumberLiteral 0 -> return t
        TNumberLiteral 1 -> return u
        TNumberLiteral _ -> throwError $ TypeError "no element at index >=2 in tuple"
        t | isSubtype t TNumber -> return (TUnion [t, u])
        _ -> return TAny -- TODO: check, not sure, gives error sometimes
    _ -> return TAny -- TODO: check, not sure, gives error sometimes

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

typeCheckBinaryOp :: TSType -> Bop -> TSType -> TSTypeChecker TSType
typeCheckBinaryOp t1 Assign t2
  | isSubtype t2 t1 = do
      _ <- updateVarEnv "x" t1
      return t2
  | otherwise = throwError $ TypeError "type mismatch"
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
typeCheckExpr' _ _ = undefined

-- | typechecks an expression
typeCheckExpr :: Expression -> TSTypeChecker TSType
typeCheckExpr = typeCheckExpr' True

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
  Either Error (Map.Map String TSType)
typeCheckProgram b = do
  runReaderT (typeCheckBlock b >> extractTopEnv) initialTSTypeEnv
  where
    extractTopEnv = do
      r <- ask
      let x = varEnvs r
      case x of
        [] -> return Map.empty
        (env : _) -> return env
