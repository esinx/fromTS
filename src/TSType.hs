module TSType where

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State qualified as S
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import TSError
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Prelude

type Name = String

type TSVarEnv = [Map String TSType]

type TSUserTypeEnv = [Map String TSType]

type TSObjectEnv = Map String TSType

data TSTypeEnv = TSTypeEnv
  { varEnvs :: TSVarEnv,
    userTypeEnvs :: TSUserTypeEnv,
    objectEnv :: TSObjectEnv
  }
  deriving (Show, Eq)

data TSType
  = TBoolean -- boolean
  | TBooleanLiteral Bool -- true, false
  | TNumber -- number
  | TNumberLiteral Double -- 1 (treated as 1.0) or 5.0
  | TString -- string
  | TStringLiteral String -- "hello"
  | TArray TSType -- Array<T> or T[]
  | TTuple [TSType] -- [T, U, ...]
  | TBracket -- {}
  | TObject -- object
  | TTypeAlias Name -- type alias
  | TUserObject (Map String TSType)
  | TUnknown -- proper top
  | TAny -- chaotic top/ bottom type
  | TNever -- bottom type
  | TVoid
  | TNull
  | TUndefined
  | TUnion [TSType]
  | TIntersection [TSType]
  deriving (Show, Eq, Ord)

(=.=) :: TSType -> TSType -> Bool
(=.=) t1 t2 = t1 `isSubtype` t2 && t2 `isSubtype` t1

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

-- | checks if a type is Truthy
isTruthy :: TSType -> Maybe Bool
isTruthy (TBooleanLiteral b) = return b
-- TODO: NaN is falsy
isTruthy (TNumberLiteral n) = return $ n /= 0
isTruthy (TStringLiteral s) = return $ not $ null s
isTruthy TNull = return False
isTruthy TUndefined = return False
isTruthy TNever = return False
isTruthy (TArray _) = return True
isTruthy (TTuple _) = return True
isTruthy TBracket = return True
isTruthy TObject = return True
isTruthy (TUserObject _) = return True
isTruthy (TUnion ts) =
  let ts' = map isTruthy ts
   in if Nothing `elem` ts'
        then Nothing
        else return $ all (== Just False) ts'
isTruthy (TIntersection ts) =
  let ts' = map isTruthy ts
   in if Nothing `elem` ts'
        then Nothing
        else return $ elem (Just True) ts'
isTruthy _ = Nothing

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
-- S1 <: T1       S2 <: T2
-- ------------------------
--    S1 * S2 <: T1 * T2
isSubtype' (TTuple t1) (TTuple u1) = length t1 == length u1 && all (uncurry isSubtype') (zip t1 u1)
-- nothing is a subtype of tuples except bottom types
isSubtype' _ (TTuple _) = False
-- nothing is a subtype of arrays except bottom types and tuples
isSubtype' (TArray t1) (TArray t2) = isSubtype' t1 t2
isSubtype' (TTuple t1) (TArray arrType) = isSubtype' (TUnion t1) arrType
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
isSubtype' (TTuple _) TObject = True
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

-- coupling: TSSyntax, to avoid circular dependencies

-- | Generate a small set of names for generated tests. These names are guaranteed to not include
-- reserved words
genNameType :: Gen String
genNameType = QC.elements ["x", "X", "y", "x0", "X0", "xy", "XY"]

-- | Generate a string literal, being careful about the characters that it may contain
genStringLitType :: Gen String
genStringLitType = escape <$> QC.listOf (QC.elements stringLitChars)
  where
    -- escape special characters appearing in the string,
    escape :: String -> String
    escape = foldr Char.showLitChar ""
    -- generate strings containing printable characters or spaces, but not including '\"'
    stringLitChars :: [Char]
    stringLitChars = filter (\c -> c /= '\"' && (Char.isSpace c || Char.isPrint c)) ['\NUL' .. '~']

-- types without data (except TAny and TNever)
basicTypes :: [TSType]
basicTypes =
  [ TBoolean,
    TNumber,
    TString,
    TBracket,
    TObject,
    TUnknown,
    TVoid,
    TNull,
    TUndefined
  ]

genMap :: Int -> Gen (Map String TSType)
genMap 0 = return Map.empty
genMap n = Map.fromList <$> QC.vectorOf 2 ((,) <$> genNameType <*> genMapType n)

genMapType :: Int -> Gen TSType
genMapType 0 =
  QC.elements basicTypes
genMapType n =
  QC.frequency
    [ (n, TArray <$> genMapType n'),
      (n, TTuple <$> QC.vectorOf 3 (genMapType n')),
      (n, TUserObject <$> genMap n'),
      (n, TUnion <$> QC.vectorOf 3 (genMapType n')),
      (n, TIntersection <$> QC.vectorOf 3 (genMapType n'))
    ]
  where
    n' = n `div` 2

genType :: Int -> Gen TSType
genType 0 =
  QC.elements $
    TAny : TNever : basicTypes
genType n =
  QC.frequency
    [ (1, TBooleanLiteral <$> arbitrary),
      (1, TNumberLiteral <$> arbitrary),
      (1, TStringLiteral <$> genStringLitType),
      (n, TArray <$> genType n'),
      (n, TTuple <$> QC.vectorOf 3 (genType n')),
      (n, TUserObject <$> genMap n'),
      (n, TUnion <$> QC.vectorOf 3 (genType n')),
      (n, TIntersection <$> QC.vectorOf 3 (genType n'))
    ]
  where
    n' = n `div` 2

genTypeExceptNever :: Int -> Gen TSType
genTypeExceptNever 0 =
  QC.elements $
    TAny : basicTypes
genTypeExceptNever n =
  QC.frequency
    [ (1, TBooleanLiteral <$> arbitrary),
      (1, TNumberLiteral <$> arbitrary),
      (1, TStringLiteral <$> genStringLitType),
      (n, TArray <$> genTypeExceptNever n'),
      (n, TTuple <$> QC.vectorOf 3 (genTypeExceptNever n')),
      (n, TUserObject <$> genMap n'),
      (n, TUnion <$> QC.vectorOf 3 (genTypeExceptNever n')),
      (n, TIntersection <$> QC.vectorOf 3 (genTypeExceptNever n'))
    ]
  where
    n' = n `div` 2

genTypeExceptAny :: Int -> Gen TSType
genTypeExceptAny 0 =
  QC.elements $
    TNever : basicTypes
genTypeExceptAny n =
  QC.frequency
    [ (1, TBooleanLiteral <$> arbitrary),
      (1, TNumberLiteral <$> arbitrary),
      (1, TStringLiteral <$> genStringLitType),
      (n, TArray <$> genTypeExceptAny n'),
      (n, TTuple <$> QC.vectorOf 3 (genTypeExceptAny n')),
      (n, TUserObject <$> genMap n'),
      (n, TUnion <$> QC.vectorOf 3 (genTypeExceptAny n')),
      (n, TIntersection <$> QC.vectorOf 3 (genTypeExceptAny n'))
    ]
  where
    n' = n `div` 2

instance Arbitrary TSType where
  arbitrary :: Gen TSType
  arbitrary = QC.sized genType

  shrink :: TSType -> [TSType]
  shrink (TArray t) = TArray <$> shrink t
  shrink (TTuple ts) = TTuple <$> shrink ts
  shrink (TUserObject m) = TUserObject <$> shrink m
  shrink (TUnion ts) = TUnion <$> shrink ts
  shrink (TIntersection ts) = TIntersection <$> shrink ts
  shrink _ = []

type TSTypeChecker = ReaderT TSTypeEnv (Either Error)

initialTSTypeEnv :: TSTypeEnv
initialTSTypeEnv =
  TSTypeEnv
    { varEnvs = [Map.empty],
      userTypeEnvs = [Map.empty],
      objectEnv = Map.empty
    }

putVarEnv :: String -> TSType -> ReaderT TSTypeEnv (Either Error) TSTypeEnv -> TSTypeChecker TSTypeEnv
putVarEnv name t comp = do
  env <- ask
  case varEnvs env of
    [] -> error "empty env" -- TODO: returning empty instead of throwing error, but this should never happen
    currEnv : envs ->
      if Map.member name currEnv
        then throwError $ TypeError $ "Repeated declaration of: " ++ name
        else local (\env -> env {varEnvs = Map.insert name t currEnv : envs}) comp

updateVarEnv :: String -> TSType -> ReaderT TSTypeEnv (Either Error) TSTypeEnv -> TSTypeChecker TSTypeEnv
updateVarEnv name t comp = do
  env <- ask
  let update [] = throwError $ TypeError $ "Variable " ++ name ++ " not found in the environment"
      update (currEnv : envs) =
        if Map.member name currEnv
          then
            local (\env -> env {varEnvs = Map.insert name t currEnv : envs}) comp
          else do
            update envs
            e <- ask
            let envs' = varEnvs e
            local (\env -> env {varEnvs = currEnv : envs'}) comp
  update (varEnvs env)

createNewVarEnv :: ReaderT TSTypeEnv (Either Error) TSTypeEnv -> TSTypeChecker TSTypeEnv
createNewVarEnv comp = do
  env <- ask
  local (\env -> env {varEnvs = Map.empty : varEnvs env}) comp

dropVarEnv :: ReaderT TSTypeEnv (Either Error) TSTypeEnv -> TSTypeChecker TSTypeEnv
dropVarEnv comp = do
  env <- ask
  case varEnvs env of
    [] -> error "empty env" -- TODO: returning empty instead of throwing error, but this should never happen
    _ : envs -> local (\env -> env {varEnvs = envs}) comp

putUserTypeEnv :: String -> TSType -> ReaderT TSTypeEnv (Either Error) TSTypeEnv -> TSTypeChecker TSTypeEnv
putUserTypeEnv name t comp = do
  env <- ask
  case userTypeEnvs env of
    [] -> error "empty env" -- TODO: returning empty instead of throwing error, but this should never happen
    currEnv : envs ->
      if Map.member name currEnv
        then throwError $ TypeError $ "Repeated declaration of: " ++ name
        else local (\env -> env {userTypeEnvs = Map.insert name t currEnv : envs}) comp

updateUserTypeEnv :: String -> TSType -> ReaderT TSTypeEnv (Either Error) TSTypeEnv -> TSTypeChecker TSTypeEnv
updateUserTypeEnv name t comp = do
  env <- ask
  let update [] = throwError $ TypeError $ "User type " ++ name ++ " not found in the environment"
      update (currEnv : envs) =
        if Map.member name currEnv
          then
            local (\env -> env {userTypeEnvs = Map.insert name t currEnv : envs}) comp
          else do
            update envs
            e <- ask
            let envs' = userTypeEnvs e
            local (\env -> env {userTypeEnvs = currEnv : envs'}) comp
  update (userTypeEnvs env)

createNewUserTypeEnv :: ReaderT TSTypeEnv (Either Error) TSTypeEnv -> TSTypeChecker TSTypeEnv
createNewUserTypeEnv comp = do
  env <- ask
  local (\env -> env {userTypeEnvs = Map.empty : userTypeEnvs env}) comp

dropUserTypeEnv :: ReaderT TSTypeEnv (Either Error) TSTypeEnv -> TSTypeChecker TSTypeEnv
dropUserTypeEnv comp = do
  env <- ask
  case userTypeEnvs env of
    [] -> error "empty env" -- TODO: returning empty instead of throwing error, but this should never happen
    _ : envs -> local (\env -> env {userTypeEnvs = envs}) comp

updateObjectEnv :: String -> TSType -> ReaderT TSTypeEnv (Either Error) TSTypeEnv -> TSTypeChecker TSTypeEnv
updateObjectEnv name t comp = do
  env <- ask
  local (\env -> env {objectEnv = Map.insert name t (objectEnv env)}) comp

lookupVarType :: String -> TSTypeChecker TSType
lookupVarType name = do
  env <- ask
  case foldr (\m acc -> Map.lookup name m <|> acc) Nothing (varEnvs env) of -- TODO: check if this impl shadows correctly
    Just t -> return t
    Nothing -> throwError $ TypeError $ "Variable " ++ name ++ " not found in the environment"

lookupUserType :: String -> TSTypeChecker TSType
lookupUserType name = do
  env <- ask
  case foldr (\m acc -> Map.lookup name m <|> acc) Nothing (userTypeEnvs env) of -- TODO: check if this impl shadows correctly
    Just t -> return t
    Nothing -> throwError $ TypeError $ "Type " ++ name ++ " not found in the environment"

lookupObjectType :: String -> TSTypeChecker TSType
lookupObjectType name = do
  env <- ask
  case Map.lookup name (objectEnv env) of
    Just t -> return t
    Nothing -> throwError $ TypeError $ "Object " ++ name ++ " not found in the environment"
