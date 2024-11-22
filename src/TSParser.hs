module TSParser where

import Control.Applicative
import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Functor
import System.IO qualified as IO
import System.IO.Error qualified as IO
import TSSyntax
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.Parsec qualified as P
import Text.Parsec.Error qualified as P
import Text.Parsec.Expr qualified as P
import Text.Parsec.Pos qualified as P
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as P
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP
import Text.Read (readMaybe)

prop_roundtrip_lit :: Literal -> Bool
prop_roundtrip_lit v = undefined

-- P.parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = undefined

-- P.parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = undefined

-- P.parse statementP (pretty s) == Right s

prop_roundtrip_block :: Block -> Bool
prop_roundtrip_block s = undefined

-- P.parse blockP (pretty s) == Right s

--------------------------------------------------------------------------------

wsP :: Parser a -> Parser a
wsP p = p <* P.spaces

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.letter) "" "a" ~?= Right 'a',
      P.parse (many (wsP P.letter)) "" "a b \n   \t c" ~?= Right "abc"
    ]

-- >>> runTestTT test_wsP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

stringP :: String -> Parser ()
stringP s = wsP (P.string s) $> ()

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "" "a" ~?= Right (),
      case P.parse (stringP "a") "" "b" of
        Left _ -> True ~?= True
        Right _ -> False ~?= True,
      P.parse (many (stringP "a")) "" "a  a" ~?= Right [(), ()]
    ]

-- >>> runTestTT test_stringP
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

constP :: String -> a -> Parser a
constP s x = wsP (P.string s) $> x

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "" "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "" "&   &" ~?= Right "aa"
    ]

-- >>> runTestTT test_constP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

parens :: Parser a -> Parser a
parens = P.between (stringP "(") (stringP ")")

braces :: Parser a -> Parser a
braces = P.between (stringP "{") (stringP "}")

brackets :: Parser a -> Parser a
brackets = P.between (stringP "[") (stringP "]")

-- >>> P.parse (many (parens (constP "1" 1))) "" "(1) (  1)   (1 )"
-- Right [1,1,1]
-- >>> P.parse (many (braces (constP "1" 1))) "" "{1} {  1}   {1 }"
-- Right [1,1,1]
-- >>> P.parse (many (brackets (constP "1" 1))) "" "[1] [  1]   [1 ]"
-- Right [1,1,1]

-- | succeed only if the input is a (positive or negative) integer
intP :: Parser Int
intP = f <$> ((++) <$> P.string "-" <*> some P.digit <|> some P.digit)
  where
    f str = case readMaybe str of
      Just x -> x
      Nothing -> error $ "Bug: can't parse '" ++ str ++ "' as an int"

literalP :: Parser Literal
literalP = intLitP <|> boolLitP <|> nullLitP <|> undefinedLitP <|> stringLitP

-- >>> P.parse (many intLitP) "" "1 2\n 3"
-- Right [IntegerLiteral 1,IntegerLiteral 2,IntegerLiteral 3]
intLitP :: Parser Literal
intLitP = wsP (IntegerLiteral <$> intP)

-- >>> P.parse (many boolLitP) "" "true false\n true"
-- Right [BooleanLiteral True,BooleanLiteral False,BooleanLiteral True]
boolLitP :: Parser Literal
boolLitP =
  BooleanLiteral <$> (True <$ stringP "true" <|> False <$ stringP "false")

-- TODO: Support nested strings and escaping
stringLitP :: Parser Literal
stringLitP =
  StringLiteral
    <$> P.between (P.char '"') (stringP "\"") (many (P.satisfy (/= '"')))
    <|> StringLiteral
      <$> P.between (P.char '\'') (stringP "'") (many (P.satisfy (/= '\'')))
    <|> StringLiteral
      <$> P.between (P.char '`') (stringP "`") (many (P.satisfy (/= '`')))

test_stringValP :: Test
test_stringValP =
  TestList
    [ P.parse stringLitP "" "\"a\"" ~?= Right (StringLiteral "a"),
      P.parse stringLitP "" "\"a\\\"\"" ~?= Right (StringLiteral "a\\"),
      P.parse (many stringLitP) "" "\"a\"   \"b\""
        ~?= Right [StringLiteral "a", StringLiteral "b"],
      P.parse (many stringLitP) "" "\" a\"   \"b\""
        ~?= Right [StringLiteral " a", StringLiteral "b"]
    ]

-- >>> runTestTT test_stringValP
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

-- >>> P.parse (many nullLitP) "" "null null\n null"
-- Right [NullLiteral,NullLiteral,NullLiteral]
nullLitP :: Parser Literal
nullLitP = NullLiteral <$ stringP "null"

-- >>> P.parse (many undefinedLitP) "" "undefined undefined\n undefined"
-- Right [UndefinedLiteral,UndefinedLiteral,UndefinedLiteral]
undefinedLitP :: Parser Literal
undefinedLitP = UndefinedLiteral <$ stringP "undefined"

expP :: Parser Expression
expP = undefined

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel = undefined

-- opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

-- >>> P.parse (many varP) "" "x y z"
-- >>> P.parse varP "" "(x.y[1]).z"
varP :: Parser Var
varP = undefined

reserved :: [String]
reserved =
  [ -- Reserved Words
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "import",
    "in",
    "instanceof",
    "new",
    "null",
    "return",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    -- Strict Mode Reserved Words
    "as",
    "implements",
    "interface",
    "let",
    "package",
    "private",
    "protected",
    "public",
    "static",
    "yield",
    -- Contextual Keywords
    "any",
    "boolean",
    "constructor",
    "declare",
    "get",
    "module",
    "require",
    "number",
    "set",
    "string",
    "symbol",
    "type",
    "from",
    "of"
  ]

-- >>> P.parse (many nameP) "" "x sfds _ nil"
-- Right ["x","sfds","_"]
nameP :: Parser Name
nameP = undefined

-- >>> P.parse (many uopPrefixP) "" "- - # --"
-- Left (line 1, column 1):
-- unexpected " "
-- expecting "--"
-- >>> P.parse (many uopPrefixP) "" "~ \n- -    ! # ++ ... typeof void +++"
-- Left (line 2, column 1):
-- unexpected " "
-- expecting "--"
uopPrefixP :: Parser UopPrefix
uopPrefixP =
  wsP $
    P.char '!' $> Not
      <|> P.char '~' $> BitNeg
      <|> P.string "typeof" $> TypeOf
      <|> P.string "..." $> Spread
      <|> P.string "--" $> DecPre
      <|> P.string "++" $> IncPre
      <|> P.char '+' $> PlusUop
      <|> P.char '-' $> MinusUop
      <|> P.string "void" $> Void

-- >>> P.parse (many uopPrefixP) "" "++   ++ -- ++      --"
-- Right [Neg,Neg,Len]
uopPostfix :: Parser UopPostfix
uopPostfix =
  wsP $
    P.string "--" $> DecPost
      <|> P.string "++" $> IncPost

-- >>> P.parse (many bopP) "" "+ >= .."
-- Right [Plus,Ge,Concat]
-- >>> P.parse (many bopP) "" ".. >>= +   <= - //  \n== % * <<===> >"
-- Right [Concat,Gt,Ge,Plus,Le,Minus,Divide,Eq,Modulo,Times,Lt,Le,Eq,Gt,Gt]
bopP :: Parser Bop
bopP = undefined

constAssignmentP :: Parser Statement
constAssignmentP = undefined

letAssignmentP :: Parser Statement
letAssignmentP = undefined

ifP :: Parser Statement
ifP = undefined

forP :: Parser Statement
forP = undefined

whileP :: Parser Statement
whileP = undefined

breakP :: Parser Statement
breakP = undefined

continueP :: Parser Statement
continueP = undefined

tryP :: Parser Statement
tryP = undefined

returnP :: Parser Statement
returnP = undefined

switchP :: Parser Statement
switchP = undefined

labeledStatementP :: Parser Statement
labeledStatementP = undefined

functionCallP :: Parser Statement
functionCallP = undefined

emptyP :: Parser Statement
emptyP = Empty <$ stringP ";"

statementP :: Parser Statement
statementP =
  constAssignmentP
    <|> letAssignmentP
    <|> ifP
    <|> forP
    <|> whileP
    <|> breakP
    <|> continueP
    <|> tryP
    <|> returnP
    <|> switchP
    <|> labeledStatementP
    <|> functionCallP
    <|> emptyP

blockP :: Parser Block
blockP = Block <$> many statementP

parseTSExp :: String -> Either P.ParseError Expression
parseTSExp = P.parse expP ""

parseTSStat :: String -> Either P.ParseError Statement
parseTSStat = P.parse statementP ""

-- | parseFromFile p filePath runs a string parser p on the input
-- read from filePath using readFile. Returns either a
-- ParseError (Left) or a value of type a (Right).
parseFromFile :: Parser a -> String -> IO (Either P.ParseError a)
parseFromFile parser filename = do
  IO.catchIOError
    ( do
        handle <- IO.openFile filename IO.ReadMode
        str <- IO.hGetContents handle
        pure $ P.parse parser str ""
    )
    ( \e ->
        pure $ Left $ P.newErrorMessage (P.Message $ show e) (P.newPos "" 0 0)
    )

parseTSFile :: String -> IO (Either P.ParseError Block)
parseTSFile = parseFromFile (const <$> blockP <*> P.eof)

tParseFiles :: Test
tParseFiles =
  "parse files"
    ~: TestList
      []
  where
    p fn ast = do
      result <- parseTSFile fn
      case result of
        (Left _) -> assert False
        (Right ast') -> assert (ast == ast')

test_comb :: Test
test_comb =
  "parsing combinators"
    ~: TestList
      [ P.parse (wsP P.letter) "" "a" ~?= Right 'a',
        P.parse (many (wsP P.letter)) "" "a b \n   \t c" ~?= Right "abc",
        P.parse (stringP "a") "" "a" ~?= Right (),
        case P.parse (stringP "a") "" "b" of
          Left _ -> True ~?= True
          Right _ -> False ~?= True,
        P.parse (many (stringP "a")) "" "a  a" ~?= Right [(), ()],
        P.parse (constP "&" 'a') "" "&  " ~?= Right 'a',
        P.parse (many (constP "&" 'a')) "" "&   &" ~?= Right "aa",
        P.parse
          (many (brackets (constP "1" 1)))
          ""
          "[1] [  1]   [1 ]"
          ~?= Right [1, 1, 1]
      ]

test_literal :: Test
test_literal =
  "parsing literals"
    ~: TestList
      [ P.parse
          (many intLitP)
          ""
          "1 2\n 3"
          ~?= Right [IntegerLiteral 1, IntegerLiteral 2, IntegerLiteral 3],
        P.parse
          (many boolLitP)
          ""
          "true false\n true"
          ~?= Right
            [ BooleanLiteral True,
              BooleanLiteral False,
              BooleanLiteral True
            ],
        P.parse
          (many nullLitP)
          ""
          "null null\n null"
          ~?= Right [NullLiteral, NullLiteral, NullLiteral],
        P.parse
          (many undefinedLitP)
          ""
          "undefined undefined\n undefined"
          ~?= Right [UndefinedLiteral, UndefinedLiteral, UndefinedLiteral],
        P.parse stringLitP "" "\"a\"" ~?= Right (StringLiteral "a"),
        P.parse stringLitP "" "\"a\\\"\"" ~?= Right (StringLiteral "a\\"),
        P.parse
          (many stringLitP)
          ""
          "\"a\"   \"b\""
          ~?= Right [StringLiteral "a", StringLiteral "b"],
        P.parse
          (many stringLitP)
          ""
          "\" a\"   \"b\""
          ~?= Right [StringLiteral " a", StringLiteral "b"]
      ]

test_exp :: Test
test_exp =
  "parsing expressions"
    ~: TestList
      []

test_stat :: Test
test_stat =
  "parsing statements"
    ~: TestList
      []

-- >>> runTestTT test_stat
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

test_all :: IO Counts
test_all =
  runTestTT $
    TestList [test_comb, test_literal, test_exp, test_stat, tParseFiles]

-- >>> test_all
-- Counts {cases = 32, tried = 32, errors = 0, failures = 0}

qc :: IO ()
qc = do
  putStrLn "roundtrip_val"
  QC.quickCheck prop_roundtrip_lit
  putStrLn "roundtrip_exp"
  QC.quickCheck prop_roundtrip_exp
  putStrLn "roundtrip_stat"
  QC.quickCheck prop_roundtrip_stat
  putStrLn "roundtrip_block"
  QC.quickCheck prop_roundtrip_block
