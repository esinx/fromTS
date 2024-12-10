module TSParser where

import Control.Applicative
import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Functor
import Data.List (foldl')
import Data.List.NonEmpty qualified as NE
import Data.Set (singleton)
import Data.Void (Void)
import System.IO qualified as IO
import System.IO.Error qualified as IO
import TSNumber
import TSSyntax
import TSType
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Error qualified as P
import Text.Megaparsec.Error.Builder qualified as P
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP
import Text.Read (readMaybe)

-- | First arg: custom error component, second arg: input stream type
type Parser = P.Parsec Void String

-- | First arg: input stream type, second arg: custom error component
type ParserError = P.ParseErrorBundle String Void

-- | Second argument of P.parse is for error messages
parse :: Parser a -> String -> Either ParserError a
parse = flip P.parse ""

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

--------------------------------------------------------------------------------

wsP :: Parser a -> Parser a
wsP p = p <* P.space

test_wsP :: Test
test_wsP =
  TestList
    [ parse (wsP P.letterChar) "a" ~?= Right 'a',
      parse (many (wsP P.letterChar)) "a b \n   \t c" ~?= Right "abc"
    ]

-- >>> runTestTT test_wsP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

stringP :: String -> Parser ()
stringP s = P.try $ wsP (P.string s) $> ()

test_stringP :: Test
test_stringP =
  TestList
    [ parse (stringP "a") "a" ~?= Right (),
      case parse (stringP "a") "b" of
        Left _ -> True ~?= True
        Right _ -> False ~?= True,
      parse (many (stringP "a")) "a  a" ~?= Right [(), ()]
    ]

-- >>> runTestTT test_stringP
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

constP :: String -> a -> Parser a
constP s x = P.try $ wsP (P.string s) $> x

test_constP :: Test
test_constP =
  TestList
    [ parse (constP "&" 'a') "&  " ~?= Right 'a',
      parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]

-- >>> runTestTT test_constP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

parens :: Parser a -> Parser a
parens = P.between (stringP "(") (stringP ")") -- TODO: Need P.try?

braces :: Parser a -> Parser a
braces = P.between (stringP "{") (stringP "}")

brackets :: Parser a -> Parser a
brackets = P.between (stringP "[") (stringP "]")

-- >>> parse (many (parens (constP "1" 1))) "(1) (  1)   (1 )"
-- Right [1,1,1]
-- >>> parse (many (braces (constP "1" 1))) "{1} {  1}   {1 }"
-- Right [1,1,1]
-- >>> parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]"
-- Right [1,1,1]

-- | Helper function to parse a negative number
negateNum :: (Num a) => Parser a -> Parser a
negateNum p = P.try $ negate <$> (P.char '-' *> p)

-- | Helper function to parse number with any sign
signed :: (Num a) => Parser a -> Parser a
signed p = negateNum p <|> p

-- | Helper function to convert a string of digits to an integer
convertBase :: Int -> String -> Int
convertBase base = foldl' (\acc c -> acc * base + Char.digitToInt c) 0

-- | Parse a positive integer (base 10)
intP :: Parser Int
intP = P.try $ convertBase 10 <$> some P.digitChar

-- | Parse a positive binary integer
binaryP :: Parser Int
binaryP = P.try $ convertBase 2 <$> (P.string' "0b" *> some P.binDigitChar)

-- | Parse a positive octal integer
octalP :: Parser Int
octalP = P.try $ convertBase 8 <$> (P.string' "0o" *> some P.octDigitChar)

-- | Parse a positive hexadecimal integer (case insensitive)
hexP :: Parser Int
hexP = P.try $ convertBase 16 <$> (P.string' "0x" *> some P.hexDigitChar)

-- | Parse a positive double
doubleP :: Parser Double
doubleP =
  P.try $
    f
      <$> many P.digitChar
      <* P.char '.'
      <*> many P.digitChar
  where
    f "" "" = error "Bug: can't parse '.' as a double"
    f "" b = val $ "0." ++ b
    f a "" = val $ a ++ ".0"
    f a b = val $ a ++ "." ++ b
    val str = case readMaybe str of
      Just x -> x
      Nothing -> error $ "Bug: can't parse '" ++ str ++ "' as a double"

-- | Parse a positive scientific number
scientificP :: Parser Double
scientificP =
  P.try $
    (\base exp -> base * 10 ^^ exp)
      <$> (doubleP <|> fromIntegral <$> intP)
      <* P.string' "e"
      <*> signed intP

-- | Parse a TS number
-- >>> parse (many (wsP numberP)) "1 0 23 -23 1.0 5.0 -5.0 5. .5 0.5 -.5 -0.5 -124. 1e1 1E1 10e1 10.e1 -.10e1 0.1e2 5E-5 2e-2 -1.0e1 -0.e2 0b1 0B1110 -0o4 -0O771 0x1 -0XFaC3 0XC0DE -Infinity Infinity NaN 5"
-- Right [1,0,23,-23,1.0,5.0,-5.0,5.0,0.5,0.5,-0.5,-0.5,-124.0,10.0,10.0,100.0,100.0,-1.0,10.0,5.0e-5,2.0e-2,-10.0,-0.0,1,14,-4,-505,1,-64195,49374,-Infinity,Infinity,NaN,5]
numberP :: Parser Number
numberP =
  wsP $
    NaN <$ (P.string "NaN" <|> P.string "-NaN")
      <|> NInfinity <$ stringP "-Infinity"
      <|> Infinity <$ stringP "Infinity"
      <|> Double <$> signed scientificP
      <|> Double <$> signed doubleP
      <|> Int <$> signed binaryP
      <|> Int <$> signed octalP
      <|> Int <$> signed hexP
      <|> Int <$> signed intP

-- >>> parse (many numberLitP) "1 2\n 3"
-- Right [NumberLiteral 1,NumberLiteral 2,NumberLiteral 3]
numberLitP :: Parser Literal
numberLitP = wsP (NumberLiteral <$> numberP)

-- >>> parse (many boolLitP) "true false\n true"
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

-- >>> parse (many nullLitP) "null null\n null"
-- Right [NullLiteral,NullLiteral,NullLiteral]
nullLitP :: Parser Literal
nullLitP = NullLiteral <$ stringP "null"

-- >>> parse (many undefinedLitP) "undefined undefined\n undefined"
-- Right [UndefinedLiteral,UndefinedLiteral,UndefinedLiteral]
undefinedLitP :: Parser Literal
undefinedLitP = UndefinedLiteral <$ stringP "undefined"

-- TODO: Support Object literals
literalP :: Parser Literal
literalP = numberLitP <|> boolLitP <|> nullLitP <|> undefinedLitP <|> stringLitP

expP :: Parser Expression
expP = undefined

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel = undefined

-- opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

-- >>> parse (many varP) "x y z"
-- >>> parse varP "(x.y[1]).z"
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

-- >>> parse (many nameP) "x sfds _ nil"
-- Right ["x","sfds","_"]
nameP :: Parser Name
nameP = undefined

typeP :: Parser TSType.TSType
typeP = undefined

-- >>> parse (many uopPrefixP) "- - ... --"
-- Left (line 1, column 1):
-- unexpected " "
-- expecting "--"
-- >>> parse (many uopPrefixP) "~ \n- -    ! # ++ ... typeof void +++"
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

-- >>> parse (many uopPrefixP) "++   ++ -- ++      --"
-- Right [Neg,Neg,Len]
uopPostfix :: Parser UopPostfix
uopPostfix =
  wsP $
    P.string "--" $> DecPost
      <|> P.string "++" $> IncPost

-- >>> parse (many bopP) "+ >= .."
-- Right [Plus,Ge,Concat]
-- >>> parse (many bopP) "|| >>= +   <= - //  \n== % * <<===> >"
-- Right [Or,Gt,Ge,Plus,Le,Minus,Divide,Eq,Modulo,Times,Lt,Le,Eq,Gt,Gt]
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

parseTSExp :: String -> Either ParserError Expression
parseTSExp = parse expP

parseTSStat :: String -> Either ParserError Statement
parseTSStat = parse statementP

-- | parseFromFile p filePath runs a string parser p on the input
-- read from filePath using readFile. Returns either a
-- ParseError (Left) or a value of type a (Right).
parseFromFile :: Parser a -> String -> IO (Either ParserError a)
parseFromFile parser filename = do
  IO.catchIOError
    ( do
        handle <- IO.openFile filename IO.ReadMode
        str <- IO.hGetContents handle
        pure $ parse parser str
    )
    ( \e ->
        pure $
          Left $
            P.ParseErrorBundle
              (P.FancyError 0 (singleton (P.ErrorFail (show e))) NE.:| [])
              (P.PosState "" 0 (P.SourcePos filename (P.mkPos 1) (P.mkPos 1)) (P.mkPos 1) "")
    )

parseTSFile :: String -> IO (Either ParserError Block)
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
      [ parse (wsP P.letterChar) "a" ~?= Right 'a',
        parse (many (wsP P.letterChar)) "a b \n   \t c" ~?= Right "abc",
        parse (stringP "a") "a" ~?= Right (),
        case parse (stringP "a") "b" of
          Left _ -> True ~?= True
          Right _ -> False ~?= True,
        parse (many (stringP "a")) "a  a" ~?= Right [(), ()],
        parse (constP "&" 'a') "&  " ~?= Right 'a',
        parse (many (constP "&" 'a')) "&   &" ~?= Right "aa",
        parse
          (many (brackets (constP "1" 1)))
          "[1] [  1]   [1 ]"
          ~?= Right [1, 1, 1]
      ]

test_literal :: Test
test_literal =
  "parsing literals"
    ~: TestList
      [ parse
          (many numberLitP)
          "1 2\n 3"
          ~?= Right [NumberLiteral 1, NumberLiteral 2, NumberLiteral 3],
        parse
          (many boolLitP)
          "true false\n true"
          ~?= Right
            [ BooleanLiteral True,
              BooleanLiteral False,
              BooleanLiteral True
            ],
        parse
          (many nullLitP)
          "null null\n null"
          ~?= Right [NullLiteral, NullLiteral, NullLiteral],
        parse
          (many undefinedLitP)
          "undefined undefined\n undefined"
          ~?= Right [UndefinedLiteral, UndefinedLiteral, UndefinedLiteral],
        parse stringLitP "\"a\"" ~?= Right (StringLiteral "a"),
        parse stringLitP "\"a\\\"\"" ~?= Right (StringLiteral "a\\"),
        parse
          (many stringLitP)
          "\"a\"   \"b\""
          ~?= Right [StringLiteral "a", StringLiteral "b"],
        parse
          (many stringLitP)
          "\" a\"   \"b\""
          ~?= Right [StringLiteral " a", StringLiteral "b"]
      ]

test_exp :: Test
test_exp =
  "parsing expressions"
    ~: TestList
      [ parse (many varP) "x y z" ~?= Right [Name "x", Name "y", Name "z"],
        parse (many nameP) "x sfds _ nil" ~?= Right ["x", "sfds", "_"],
        parse (many uopPrefixP) "- - ... --" ~?= Right [MinusUop, MinusUop, Spread, DecPre],
        parse (many bopP) "+ >= ||" ~?= Right [PlusBop, Ge, Or]
      ]

test_stat :: Test
test_stat =
  "parsing statements"
    ~: TestList
      [ parse statementP "const x = 3" ~?= Right (ConstAssignment (Name "x") (Lit (NumberLiteral 3))),
        parse statementP "if (x) { let y = undefined } else { const y = null }"
          ~?= Right
            ( If
                (Var (Name "x"))
                (Block [LetAssignment (Name "y") (Lit UndefinedLiteral)])
                (Block [ConstAssignment (Name "y") (Lit NullLiteral)])
            )
            -- parse statementP "while (null) { x += 1 }"
            --   ~?= Right
            --     ( While
            --         (Lit NullLiteral)
            --         (Block [PlusBop (Var (Name "x")) (Lit (IntegerLiteral 1))])
            --     )
      ]

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
