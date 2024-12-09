import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State qualified as S
import Data.Map qualified as Map
import GHC.IO (unsafePerformIO)
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import TSError
import TSParser
import TSSyntax
import TSType
import TSTypeChecker
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  -- typechecker
  runTestTT test_typeChecker
  putStrLn "subtypeReflexive"
  quickCheckN 100 prop_subtypeReflexive
  putStrLn "properBottomTypeNever"
  quickCheckN 100 prop_properBottomTypeNever
  putStrLn "properTopTypeUnknown"
  quickCheckN 100 prop_properTopTypeUnknown
  putStrLn "chaoticTopTypeAny"
  quickCheckN 100 prop_chaoticTopTypeAny
  putStrLn "chaoticBottomTypeAny"
  quickCheckN 100 prop_chaoticBottomTypeAny
  putStrLn "asymmetricExceptAny"
  quickCheckN 100 prop_asymmetricExceptAny
  putStrLn "transitiveExceptAny"
  quickCheckN 100 prop_transitiveExceptAny
  putStrLn "func"
  quickCheckN 100 prop_func
  putStrLn "differential"
  quickCheckN 100 prop_differential

-- unit tests for the typechecker
test_typeChecker :: Test
test_typeChecker =
  TestList
    [ test_subtyping,
      test_typeCheckExpr,
      test_typeCheckStmt
    ]

test_subtyping :: Test
test_subtyping =
  "subtyping tests"
    ~: TestList
      [ isSubtype
          (TUnion [TBoolean, TNumber])
          (TUnion [TBoolean, TNumber, TUndefined])
          ~?= True,
        isSubtype
          (TIntersection [TBoolean, TNumber])
          TBoolean
          ~?= True,
        isSubtype TUndefined TVoid ~?= True,
        isSubtype (TBooleanLiteral True) TBoolean ~?= True,
        isSubtype (TStringLiteral "hi") TString ~?= True,
        isSubtype (TNumberLiteral 4) TNumber ~?= True,
        isSubtype TBoolean TBracket ~?= True,
        isSubtype TBoolean TObject ~?= False,
        isSubtype (TArray TNumber) TObject ~?= True,
        isSubtype (TArray TNumber) TBracket
          ~?= True,
        isSubtype
          (TTuple TNumber TBoolean)
          ( TArray
              (TUnion [TNumber, TBoolean])
          )
          ~?= True,
        isSubtype
          (TTuple TNumber TBoolean)
          ( TArray
              (TUnion [TNumber, TString])
          )
          ~?= False,
        isSubtype TObject TBracket ~?= True,
        isSubtype (TUserObject (Map.fromList [("x", TNumber)])) TObject
          ~?= True,
        isSubtype (TFunction [] TNumber) TObject
          ~?= True,
        isSubtype (TFunction [TAny] TNever) (TFunction [TNull] TUnknown)
          ~?= True,
        isSubtype TAny (TIntersection [TIntersection [TNever]])
          ~?= False,
        isSubtype (TUnion [TBracket, TNull, TUndefined]) TUnknown
          ~?= True,
        isSubtype (TUnion [TNull, TBracket, TUndefined]) TUnknown
          ~?= True,
        isSubtype
          (TUserObject (Map.fromList [("x", TNumber), ("y", TBoolean)]))
          (TUserObject (Map.fromList [("x", TNumber)]))
          ~?= True
      ]

test_typeCheckExpr :: Test
test_typeCheckExpr =
  "expression type checking tests"
    ~: TestList
      [ runReaderT (typeCheckExpr (Lit (BooleanLiteral True))) initialTSTypeEnv
          ~?= Right TBoolean
      ]

test_typeCheckStmt :: Test
test_typeCheckStmt =
  "statement type checking tests"
    ~: TestList
      [ runReaderT
          ( typeCheckStmt
              ( LetAssignment
                  (Name "x")
                  Nothing
                  (Lit (BooleanLiteral True))
              )
          )
          initialTSTypeEnv
          ~?= Right ()
      ]

-- properties for the typechecker
prop_subtypeReflexive :: TSType -> Bool
prop_subtypeReflexive t = isSubtype t t

prop_properBottomTypeNever :: TSType -> Bool
-- never is assignable to anything
prop_properBottomTypeNever = isSubtype TNever

prop_properTopTypeUnknown :: TSType -> Bool
-- Anything is assignable to unknown
prop_properTopTypeUnknown t = isSubtype t TUnknown

prop_chaoticTopTypeAny :: TSType -> Bool
--- Anything is assignable to any
prop_chaoticTopTypeAny t = isSubtype t TAny

prop_chaoticBottomTypeAny :: TSType -> Bool
-- any is assignable to anything (except never)
prop_chaoticBottomTypeAny t | simplify t == TNever = not $ isSubtype TAny TNever
prop_chaoticBottomTypeAny t = isSubtype TAny t

prop_asymmetricExceptAny :: TSType -> TSType -> Property
prop_asymmetricExceptAny t1 t2 =
  simplify t1 /= TAny && simplify t2 /= TAny && simplify t1 /= simplify t2 ==>
    (not (isSubtype t1 t2) || not (isSubtype t2 t1))

prop_transitiveExceptAny :: TSType -> TSType -> TSType -> Property
prop_transitiveExceptAny t1 t2 t3 =
  isSubtype t1 t2
    && isSubtype t2 t3
    && simplify t1 /= TAny
    && simplify t2 /= TAny
    && simplify t3 /= TAny
    ==> isSubtype t1 t3

prop_func :: TSType -> TSType -> TSType -> TSType -> Property
prop_func s1 s2 t1 t2 =
  isSubtype t1 s1 && isSubtype s2 t2 ==>
    isSubtype (TFunction [s1] s2) (TFunction [t1] t2)

prop_differential :: Block -> Property
prop_differential = ioProperty . prop_ioDifferential

prop_ioDifferential :: Block -> IO Property
prop_ioDifferential b =
  let outputFile = "test/output.ts"
      typeCheckResult = typeCheckProgram b
   in do
        writeFile outputFile $ pretty b
        result <- readProcessWithExitCode "tsc" [outputFile] ""
        return $ property $ match result typeCheckResult
  where
    match (ExitSuccess, _, _) (Right _) = True
    match (ExitFailure _, _, _) (Left _) = True
    match _ _ = False