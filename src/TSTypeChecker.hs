module TSTypeChecker where

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

-- | checks if a type is a subtype of another type
isSubtype :: TSType -> TSType -> Bool
-- reflexivity
isSubtype t1 t2 | simplify t1 == simplify t2 = True
-- union
isSubtype (TUnion ts) t = all (`isSubtype` t) ts
isSubtype t (TUnion ts) = any (isSubtype t) ts
-- intersection
isSubtype (TIntersection ts) t = any (`isSubtype` t) ts
isSubtype t (TIntersection ts) = all (isSubtype t) ts
-- proper bottom type
isSubtype TNever _ = True
-- proper top type
isSubtype _ TUnknown = True
-- chaotic top/ bottom type
isSubtype _ TAny = True
isSubtype TAny TNever = False
isSubtype TAny _ = True
-- nothing is a subtype of null except bottom types
isSubtype _ TNull = False
-- nothing is a subtype of undefined except bottom types
isSubtype _ TUndefined = False
-- nothing is a subtype of void except bottom types and undefined
isSubtype TUndefined TVoid = True
isSubtype _ TVoid = False
-- nothing is a subtype of literal types except bottom types
isSubtype _ (TStringLiteral _) = False
isSubtype _ (TNumberLiteral _) = False
isSubtype _ (TBooleanLiteral _) = False
-- T1 <: S1       S2 <: T2
-- ----------------------
--    S1 → S2 <: T1 → T2
isSubtype (TFunction args1 ret1) (TFunction args2 ret2) =
  length args1 <= length args2 -- contravariant
    && all (uncurry isSubtype) (zip args2 args1)
    && isSubtype ret1 ret2
-- nothing is a subtype of functions except bottom types
isSubtype _ (TFunction _ _) = False
-- S1 <: T1       S2 <: T2
-- ------------------------
--    S1 * S2 <: T1 * T2
isSubtype (TTuple t1 t2) (TTuple u1 u2) = isSubtype t1 u1 && isSubtype t2 u2
-- nothing is a subtype of tuples except bottom types
isSubtype _ (TTuple _ _) = False
-- nothing is a subtype of arrays except bottom types and tuples
isSubtype (TArray t1) (TArray t2) = isSubtype t1 t2
isSubtype (TTuple t1 t2) (TArray arrType) = isSubtype (TUnion [t1, t2]) arrType
isSubtype _ (TArray _) = False
--              n > m
-- ---------------------------------- (Width Subtyping)
-- {i1:T1...in:Tn} <: {i1:T1...im:Tm}
--      S1 <: T1  ...  Sn <: Tn
-- ---------------------------------- (Depth Subtyping)
-- {i1:S1...in:Sn} <: {i1:T1...in:Tn}
-- object 1's fields is a permutation of fields in object 2
isSubtype (TUserObject n) (TUserObject m) =
  length n >= length m
    && all
      ( \(mKey, mType) ->
          case Map.lookup mKey n of
            Just nType -> isSubtype nType mType
            Nothing -> False
      )
      (Map.toList m)
-- nothing is a subtype of user objects except bottom types
isSubtype _ (TUserObject _) = False
-- bottom types, function, tuple, array, user objects are subtypes of object
isSubtype (TFunction _ _) TObject = True
isSubtype (TTuple _ _) TObject = True
isSubtype (TArray _) TObject = True
isSubtype (TUserObject _) TObject = True
isSubtype _ TObject = False
-- bottom types, string literal are subtypes of string
isSubtype (TStringLiteral _) TString = True
isSubtype _ TString = False
-- bottom types, number literal are subtypes of number
isSubtype (TNumberLiteral _) TNumber = True
isSubtype _ TNumber = False
-- bottom types, boolean literal are subtypes of boolean
isSubtype (TBooleanLiteral _) TBoolean = True
isSubtype _ TBoolean = False
-- everything except null, void, undefined, unknown are subtypes of bracket
isSubtype t TBracket
  | t == TNull || t == TVoid || t == TUndefined || t == TUnknown = False
  | otherwise = True
isSubtype _ _ = False

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
