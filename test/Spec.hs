import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State qualified as S
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Map as Map
import GHC.IO (unsafePerformIO)
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Model
import System.Process (readProcessWithExitCode)
import TSError
import TSParser
import TSSyntax
import TSType
import TSTypeChecker
import Test.HUnit
import Test.QuickCheck
import Prelude

matchTypeMap :: Map String TSType -> Map String TSType -> Bool
matchTypeMap truthTypeMap typeMap =
  all
    ( \(name, t) -> case Map.lookup name truthTypeMap of
        Just t' -> t =.= t'
        Nothing -> False
    )
    (Map.toAscList typeMap)

main :: IO ()
main = do
  -- typechecker
  putStrLn "test_model"
  runTestTT test_model
  putStrLn "--- All tests complete ---"

-- runTestTT test_typeChecker
-- putStrLn "subtypeReflexive"
-- quickCheckN 100 prop_subtypeReflexive
-- putStrLn "properBottomTypeNever"
-- quickCheckN 100 prop_properBottomTypeNever
-- putStrLn "properTopTypeUnknown"
-- quickCheckN 100 prop_properTopTypeUnknown
-- putStrLn "chaoticTopTypeAny"
-- quickCheckN 100 prop_chaoticTopTypeAny
-- putStrLn "chaoticBottomTypeAny"
-- quickCheckN 100 prop_chaoticBottomTypeAny
-- putStrLn "asymmetricExceptAny"
-- quickCheckN 100 prop_asymmetricExceptAny
-- putStrLn "transitiveExceptAny"
-- quickCheckN 100 prop_transitiveExceptAny
-- putStrLn "func"
-- quickCheckN 100 prop_func
-- putStrLn "differential"
-- quickCheckN 100 prop_differential

-- unit tests for the typechecker
test_typeChecker :: Test
test_typeChecker =
  TestList
    [ test_subtyping,
      test_simplify,
      test_typeCheckExpr,
      test_typeCheckStmt,
      test_typeCheckProg
    ]

test_simplify :: Test
test_simplify =
  "simplify tests"
    ~: TestList
      [ simplify
          (TIntersection [TIntersection [TIntersection [TIntersection [TIntersection [TIntersection [TNever]]]]]])
          ~?= TNever
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
          (TTuple [TNumber, TBoolean])
          ( TArray
              (TUnion [TNumber, TBoolean])
          )
          ~?= True,
        isSubtype
          (TTuple [TNumber, TBoolean])
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
          ~?= Right (TBooleanLiteral True),
        runReaderT (typeCheckExpr (Var (Name "x"))) initialTSTypeEnv {varEnvs = [Map.singleton "x" TNumber]}
          ~?= Right TNumber
      ]

test_typeCheckStmt :: Test
test_typeCheckStmt =
  "statement type checking tests"
    ~: TestList
      [ runReaderT
          ( typeCheckStmt
              ( LetAssignment
                  (Name "x")
                  (Lit (BooleanLiteral True))
              )
              Nothing
              ask
          )
          initialTSTypeEnv
          ~?= Right (initialTSTypeEnv {varEnvs = [Map.singleton "x" TBoolean]})
      ]

test_typeCheckProg :: Test
test_typeCheckProg =
  "program type checking tests"
    ~: TestList
      [ -- let x = true; let y = x;
        typeCheckProgram
          ( Block
              [ LetAssignment
                  (Name "x")
                  (Lit (BooleanLiteral True)),
                LetAssignment
                  (Name "y")
                  (Var (Name "x"))
              ]
          )
          ~?= Right (Map.fromList [("x", TBoolean), ("y", TBoolean)]),
        -- let x = true; if (x) { let y = true; } else { let y = false; }
        typeCheckProgram
          ( Block
              [ LetAssignment
                  (Name "x")
                  (Lit (BooleanLiteral True)),
                If
                  [ ( Var (Name "x"),
                      Block [LetAssignment (Name "y") (Lit (BooleanLiteral True))]
                    )
                  ]
                  (Block [LetAssignment (Name "y") (Lit (BooleanLiteral False))])
              ]
          )
          ~?= Right (Map.fromList [("x", TBoolean)]),
        -- shadowing
        typeCheckProgram
          ( Block
              [ LetAssignment
                  (Name "x")
                  (Lit (BooleanLiteral True)),
                If
                  [ ( Var (Name "x"),
                      Block
                        [ LetAssignment (Name "x") (Lit (StringLiteral "hi")),
                          LetAssignment
                            (Name "y")
                            (AnnotatedExpression TString (Var (Name "x")))
                        ]
                    )
                  ]
                  (Block [])
              ]
          )
          ~?= Right (Map.fromList [("x", TBoolean)]),
        typeCheckProgram
          ( Block
              [ LetAssignment
                  (Name "x")
                  (Lit (BooleanLiteral True)),
                If
                  [ ( Var (Name "x"),
                      Block
                        [ LetAssignment
                            (Name "y")
                            (AnnotatedExpression TString (Var (Name "x")))
                        ]
                    )
                  ]
                  (Block [])
              ]
          )
          ~?= Left (TypeError "type mismatch"),
        -- repeated declaration
        typeCheckProgram
          ( Block
              [ LetAssignment
                  (Name "x")
                  (Lit (BooleanLiteral True)),
                LetAssignment
                  (Name "x")
                  (Lit (BooleanLiteral False))
              ]
          )
          ~?= Left (TypeError $ "Repeated declaration of: " ++ "x"),
        -- let x = true; let y: string = x;
        typeCheckProgram
          ( Block
              [ LetAssignment
                  (Name "x")
                  (Lit (BooleanLiteral True)),
                LetAssignment
                  (Name "y")
                  (AnnotatedExpression TString (Var (Name "x")))
              ]
          )
          ~?= Left (TypeError "type mismatch")
      ]

test_model :: Test
test_model =
  "model based tests"
    ~: TestList
    $ fmap
      (\fileName -> fileName ~: p fileName)
      [ "./test/const-literals.ts",
        "./test/variables.ts"
      ]
  where
    p :: String -> IO ()
    p fileName = do
      result <-
        runModelTypeChecker fileName
      parsed <- parseTSFile fileName
      case (result, parsed) of
        (Nothing, _) -> Test.HUnit.assert False
        (Just truthTypeMap, Right ts) ->
          case typeCheckProgram ts of
            Left _ -> Test.HUnit.assert False
            Right typeMap -> Test.HUnit.assert $ matchTypeMap truthTypeMap typeMap
        (_, Left err) -> Test.HUnit.assert False

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

prop_chaoticBottomTypeAny :: Property
-- any is assignable to anything (except never)
prop_chaoticBottomTypeAny = forAll (sized genTypeExceptNever) prop_chaoticBottomTypeAny'
  where
    prop_chaoticBottomTypeAny' = isSubtype TAny

prop_transitiveExceptAny :: Property
prop_transitiveExceptAny =
  forAll
    (sized genTypeExceptAny)
    ( \t1 ->
        forAll
          (sized genTypeExceptAny)
          ( forAll
              (sized genTypeExceptAny)
              . prop_transitiveExceptAny' t1
          )
    )
  where
    prop_transitiveExceptAny' t1 t2 t3 =
      isSubtype t1 t2 && isSubtype t2 t3 ==> isSubtype t1 t3

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
        result <-
          readProcessWithExitCode
            "node"
            ["./model/dist/index.js", outputFile]
            []
        return $ property $ match result typeCheckResult
  where
    match :: (ExitCode, String, String) -> Either Error (Map String TSType) -> Bool
    match (ExitSuccess, stdout, _) (Right typeMap) =
      let truthTypeMap = JSON.decode $ BLU.fromString stdout :: Maybe (Map String String)
       in case truthTypeMap of
            Just tm ->
              let entries = Map.toAscList typeMap
               in all
                    ( \(name, t) ->
                        case Map.lookup name tm of
                          Just t' -> case parse typeP t' of
                            Right t'' -> t =.= t''
                            Left _ -> False
                          Nothing -> False
                    )
                    entries
            Nothing -> False
    match (ExitFailure _, _, _) (Left _) = True
    match _ _ = False