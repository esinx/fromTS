module TSType where

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State qualified as S
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import TSError
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

type TSGlobalEnv = Map String TSType

type TSLocalEnv = Map String TSType

type TSObjectEnv = Map String TSType

data TSTypeEnv = TSTypeEnv
  { globalEnv :: TSGlobalEnv,
    localEnv :: TSLocalEnv,
    objectEnv :: TSObjectEnv
  }
  deriving (Show, Eq)

data TSType
  = TBoolean -- boolean
  | TBooleanLiteral Bool -- true, false
  | TNumber -- number
  | TNumberLiteral Int -- 1
  | TString -- string
  | TStringLiteral String -- "hello"
  | TArray TSType -- Array<T>
  | TTuple TSType TSType -- [T, U]
  | TBracket -- {}
  | TObject -- object
  | TUserObject (Map String TSType)
  | TFunction [TSType] TSType
  | TUnknown -- proper top
  | TAny -- chaotic top/ bottom type
  | TNever -- bottom type
  | TVoid
  | TNull
  | TUndefined
  | TUnion [TSType]
  | TIntersection [TSType]
  deriving (Show, Eq, Ord)

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

genMap :: Int -> Gen (Map String TSType)
genMap 0 = return Map.empty
genMap n = Map.fromList <$> QC.vectorOf 2 ((,) <$> genNameType <*> genType n)

genType :: Int -> Gen TSType
genType 0 =
  QC.elements
    [ TBoolean,
      TNumber,
      TString,
      TBracket,
      TObject,
      TUnknown,
      TAny,
      TNever,
      TVoid,
      TNull,
      TUndefined
    ]
genType n =
  QC.frequency
    [ (1, TBooleanLiteral <$> arbitrary),
      (1, TNumberLiteral <$> arbitrary),
      (1, TStringLiteral <$> genStringLitType),
      (n, TArray <$> genType n'),
      (n, TTuple <$> genType n' <*> genType n'),
      (n, TUserObject <$> genMap n'),
      (n, TFunction <$> QC.vectorOf 2 (genType n') <*> genType n'),
      (n, TUnion <$> QC.vectorOf 3 (genType n')),
      (n, TIntersection <$> QC.vectorOf 3 (genType n'))
    ]
  where
    n' = n `div` 2

instance Arbitrary TSType where
  arbitrary = QC.sized genType
  shrink :: TSType -> [TSType]
  shrink (TArray t) = TArray <$> shrink t
  shrink (TTuple t u) = TTuple <$> shrink t <*> shrink u
  shrink (TUserObject m) = TUserObject <$> shrink m
  shrink (TFunction ts t) = TFunction <$> shrink ts <*> shrink t
  shrink (TUnion ts) = TUnion <$> shrink ts
  shrink (TIntersection ts) = TIntersection <$> shrink ts
  shrink _ = []

type TSTypeChecker = ReaderT TSTypeEnv (Either Error)

initialTSTypeEnv :: TSTypeEnv
initialTSTypeEnv =
  TSTypeEnv
    { globalEnv = Map.empty,
      localEnv = Map.empty,
      objectEnv = Map.empty
    }

updateGlobalEnv :: String -> TSType -> TSTypeChecker ()
updateGlobalEnv name t = do
  env <- ask
  local (\env -> env {objectEnv = Map.insert name t (globalEnv env)}) (return ())

updateLocalEnv :: String -> TSType -> TSTypeChecker ()
updateLocalEnv name t = do
  env <- ask
  local (\env -> env {objectEnv = Map.insert name t (localEnv env)}) (return ())

updateObjectEnv :: String -> TSType -> TSTypeChecker ()
updateObjectEnv name t = do
  env <- ask
  local (\env -> env {objectEnv = Map.insert name t (objectEnv env)}) (return ())

lookupVarType :: String -> TSTypeChecker TSType
lookupVarType name = do
  env <- ask
  case Map.lookup name (localEnv env) <|> Map.lookup name (globalEnv env) of
    Just t -> return t
    Nothing -> throwError $ TypeError $ "Variable " ++ name ++ " not found in the environment"

lookupObjectType :: String -> TSTypeChecker TSType
lookupObjectType name = do
  env <- ask
  case Map.lookup name (objectEnv env) of
    Just t -> return t
    Nothing -> throwError $ TypeError $ "Object " ++ name ++ " not found in the environment"