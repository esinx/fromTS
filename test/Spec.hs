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

prop_supertypeNever :: TSType -> Bool
prop_supertypeNever = isSubtype TNever

-- TODO: clarify the subtyping relations
prop_subtypeVoid :: TSType -> Bool
prop_subtypeVoid = isSubtype TVoid

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