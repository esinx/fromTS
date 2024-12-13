import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State qualified as S
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Map as Map
import Debug.Trace
import GHC.IO (unsafePerformIO)
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import GHC.IO.Handle
import Model
import System.IO.Temp (withSystemTempFile, withTempFile)
import System.Process (readProcessWithExitCode)
import TSError
import TSGen
import TSNumber
import TSParser
import TSSyntax
import TSType
import TSTypeChecker
import Test.HUnit
import Test.QuickCheck
  ( Property,
    Testable (property),
    forAll,
    ioProperty,
    sized,
    (==>),
  )
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
  -- parser
  putStrLn "test parser"
  test_all
  putStrLn "roundtrip_val"
  quickCheckN 100 prop_roundtrip_lit
  putStrLn "roundtrip_exp"
  quickCheckN 100 prop_roundtrip_exp
  putStrLn "roundtrip_stat"
  quickCheckN 100 prop_roundtrip_stat
  putStrLn "roundtrip_block"
  quickCheckN 100 prop_roundtrip_block
  -- typechecker
  putStrLn "test_model"
  runTestTT test_model
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
  putStrLn "transitiveExceptAny"
  quickCheckN 100 prop_transitiveExceptAny
  putStrLn "differential"
  quickCheckN 1000 prop_differential
  putStrLn "--- All tests complete ---"

-- unit tests for the parser
test_all :: IO Counts
test_all =
  runTestTT $
    TestList [test_comb, test_literal, test_exp, test_stat, tParseFiles]

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
                            (AnnotatedExpression (TSTypeWrapper TString) (Var (Name "x")))
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
                            (AnnotatedExpression (TSTypeWrapper TString) (Var (Name "x")))
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
                  (AnnotatedExpression (TSTypeWrapper TString) (Var (Name "x")))
              ]
          )
          ~?= Left (TypeError "type mismatch"),
        -- const x = -1;
        typeCheckProgram
          ( Block
              [ ConstAssignment
                  (Name "x")
                  (UnaryOpPrefix MinusUop (Lit (NumberLiteral (Double 1))))
              ]
          )
          ~?= Right (Map.fromList [("x", TNumberLiteral (-1))]),
        typeCheckProgram
          ( Block
              [ TypeAlias
                  "X"
                  (TSTypeWrapper (TUnion [TNumber, TString])),
                ConstAssignment
                  (Name "x")
                  (AnnotatedExpression (TSTypeWrapper (TTypeAlias "X")) (Lit (NumberLiteral (Double 1)))),
                AnyExpression (BinaryOp (Var (Name "x")) Assign (Lit (StringLiteral "hi")))
              ]
          )
          ~?= Right (Map.fromList [("x", TTypeAlias "X")])
      ]

compareToModel :: String -> IO Bool
compareToModel fileName = do
  result <- runModelTypeChecker fileName
  parsed <- parseTSFile fileName
  case (result, parsed) of
    (Nothing, Left _) -> return True
    (Nothing, _) -> return False
    (Just truthTypeMap, Right ts) ->
      case typeCheckProgram ts of
        Left _ -> return False
        Right typeMap ->
          return $
            matchTypeMap truthTypeMap typeMap
    (_, Left err) ->
      return False

compareToModel' :: String -> IO Bool
compareToModel' source =
  withTempFile "./tmp" "tempFile.ts" $ \outputFile handle -> do
    hPutStr handle source
    hFlush handle
    result <- runModelTypeChecker outputFile
    let parsed = parseTSSource source
    hClose handle
    case (result, parsed) of
      (Just truthTypeMap, Right ts) ->
        case typeCheckProgram ts of
          Left _ -> return False
          Right typeMap -> return $ matchTypeMap truthTypeMap typeMap
      (Nothing, Left _) -> return True
      (Nothing, Right ts) ->
        case typeCheckProgram ts of
          Left _ -> return True
          Right _ -> return False
      (_, Left err) -> return False

test_model :: Test
test_model =
  "model based tests"
    ~: TestList
    $ fmap
      (\fileName -> fileName ~: p fileName)
      [ "./test/const-literals.ts",
        "./test/variables.ts",
        "./test/object.ts",
        "./test/bfs.ts"
      ]
  where
    p :: String -> IO ()
    p fileName = do
      result <- compareToModel fileName
      assertBool fileName result

-- properties for the parser

-- | Tests parsing the pretty printed value of a literal, should match
prop_roundtrip_lit :: Literal -> Bool
prop_roundtrip_lit v = parse literalP (pretty v) == Right v

-- | Tests parsing the pretty printed value of an expression, should match
prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = parse expP (pretty e) == Right e

-- | Tests parsing the pretty printed value of a statement, should match
prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = parse statementP (pretty s) == Right s

-- | Tests parsing the pretty printed value of a block, should match
prop_roundtrip_block :: Block -> Bool
prop_roundtrip_block s = parse blockP (pretty s) == Right s

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

prop_differential :: Block -> Property
prop_differential b = ioProperty $ prop_ioDifferential b

prop_ioDifferential :: Block -> IO Property
prop_ioDifferential b = do
  passed <- compareToModel' (pretty b)
  return $ property passed
