module TSParser where

import Control.Applicative
import Control.Monad (guard, mapM_)
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

filterP :: (a -> Bool) -> Parser a -> Parser a
filterP f p = P.try $ p >>= \x -> if f x then pure x else fail "Predicate not satisfied"

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
wsP p = P.try $ p <* P.space

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
parens = P.between (stringP "(") (stringP ")")

braces :: Parser a -> Parser a
braces = P.between (stringP "{") (stringP "}")

brackets :: Parser a -> Parser a
brackets = P.between (stringP "[") (stringP "]")

abrackets :: Parser a -> Parser a
abrackets = P.between (stringP "<") (stringP ">")

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
-- >>> parse (many (wsP numberValP)) "1 0 23 -23 1.0 5.0 -5.0 5. .5 0.5 -.5 -0.5 -124. 1e1 1E1 10e1 10.e1 -.10e1 0.1e2 5E-5 2e-2 -1.0e1 -0.e2 0b1 0B1110 -0o4 -0O771 0x1 -0XFaC3 0XC0DE -Infinity Infinity NaN 5"
-- Right [1,0,23,-23,1.0,5.0,-5.0,5.0,0.5,0.5,-0.5,-0.5,-124.0,10.0,10.0,100.0,100.0,-1.0,10.0,5.0e-5,2.0e-2,-10.0,-0.0,1,14,-4,-505,1,-64195,49374,-Infinity,Infinity,NaN,5]
numberValP :: Parser Number
numberValP =
  wsP
    ( NaN <$ (P.try (P.string "NaN") <|> P.try (P.string "-NaN"))
        <|> P.try (NInfinity <$ stringP "-Infinity")
        <|> P.try (Infinity <$ stringP "Infinity")
        <|> Double <$> signed scientificP
        <|> Double <$> signed doubleP
        <|> Int <$> signed binaryP
        <|> Int <$> signed octalP
        <|> Int <$> signed hexP
        <|> Int <$> signed intP
    )

-- >>> parse (many boolValP) "true false\n true"
-- Right [True,False,True]
boolValP :: Parser Bool
boolValP = P.try (True <$ stringP "true" <|> False <$ stringP "false")

-- TODO: Support nested strings and escaping
stringValP :: Parser String
stringValP =
  P.try (P.between (P.char '"') (stringP "\"") (many (P.satisfy (/= '"'))))
    <|> P.try (P.between (P.char '\'') (stringP "'") (many (P.satisfy (/= '\''))))
    <|> P.try (P.between (P.char '`') (stringP "`") (many (P.satisfy (/= '`'))))

test_stringValP :: Test
test_stringValP =
  TestList
    [ parse stringValP "\"a\"" ~?= Right ("a"),
      parse stringValP "\"a\\\"\"" ~?= Right ("a\\"),
      parse (many stringValP) "\"a\"   \"b\""
        ~?= Right ["a", "b"],
      parse (many stringValP) "\" a\"   \"b\""
        ~?= Right [" a", "b"]
    ]

-- >>> runTestTT test_stringValP
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

-- TODO: Support Object literals
literalP :: Parser Literal
literalP =
  wsP
    ( NumberLiteral <$> numberValP
        <|> BooleanLiteral <$> boolValP
        <|> P.try (NullLiteral <$ stringP "null")
        <|> P.try (UndefinedLiteral <$ stringP "undefined")
        <|> StringLiteral <$> stringValP
    )

arrayTypeP :: Parser TSType
arrayTypeP = P.try (stringP "Array" *> abrackets typeP) <|> P.try (TArray <$> (typeP <* brackets (pure ())))

tupleP :: Parser [TSType]
tupleP = P.try $ brackets (typeP `P.sepBy` stringP ",")

unionP :: Parser [TSType]
unionP = P.try $ typeP `P.sepBy` stringP "|"

intersectionP :: Parser [TSType]
intersectionP = P.try $ typeP `P.sepBy` stringP "&"

-- TODO: Add bracket, object, UserObject, function
typeP :: Parser TSType
typeP =
  TBooleanLiteral <$> boolValP
    <|> TBoolean <$ stringP "boolean"
    <|> TNumberLiteral <$> signed scientificP
    <|> TNumberLiteral <$> signed doubleP
    <|> TNumberLiteral . fromIntegral <$> signed binaryP
    <|> TNumberLiteral . fromIntegral <$> signed octalP
    <|> TNumberLiteral . fromIntegral <$> signed hexP
    <|> TNumberLiteral . fromIntegral <$> signed intP
    <|> TNumber <$ stringP "number"
    <|> TStringLiteral <$> stringValP
    <|> TString <$ stringP "string"
    <|> TArray <$> arrayTypeP
    <|> TUnion <$> tupleP -- TODO: FIX TUPLES
    <|> TUnknown <$ stringP "unknown"
    <|> TAny <$ stringP "any"
    <|> TNever <$ stringP "never"
    <|> TVoid <$ stringP "void"
    <|> TNull <$ stringP "null"
    <|> TUndefined <$ stringP "undefined"
    <|> TUnion <$> unionP -- TODO: FIX UNION AND INTERSECTION PRIORITY/PRECEDENCE
    <|> TIntersection <$> intersectionP

baseExpP :: Parser Expression
baseExpP =
  wsP
    ( P.try (Lit <$> literalP)
        <|> P.try (Array <$> brackets (expP `P.sepBy` stringP ","))
        <|> P.try (Var <$> varP)
        <|> P.try (parens expP)
    )

annotatedExpP :: Parser Expression
annotatedExpP = P.try $ do
  e <- baseExpP
  me <- optional (stringP ":" *> typeP)
  case me of
    Just t -> return (AnnotatedExpression t e)
    Nothing -> return e

prefixExpP :: Parser Expression
prefixExpP =
  P.try (UnaryOpPrefix <$> uopPrefixP <*> prefixExpP) <|> P.try annotatedExpP

postfixExpP :: Parser Expression
postfixExpP = P.try $ do
  e <- prefixExpP
  me <- optional uopPostfix
  case me of
    Just op -> return (UnaryOpPostfix e op)
    Nothing -> return e

-- | Parses one or more occurrences of @p@ separated by binary operator
-- parser @pop@.  Returns a value produced by a /left/ associative application
-- of all functions returned by @pop@.
-- See the end of the `Parsers` lecture for explanation of this operator.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` pop = foldl comb <$> p <*> rest
  where
    comb x (op, y) = x `op` y
    rest = many ((,) <$> pop <*> p)

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip BinaryOp <$> filterP (\x -> level x == l) bopP

chainLevel :: Parser Expression -> Int -> Parser Expression
chainLevel sub l = sub `chainl1` opAtLevel l

expP :: Parser Expression
expP =
  let exp14 = chainLevel postfixExpP 14 -- exp
      exp13 = chainLevel exp14 13 -- mult., /, %
      exp12 = chainLevel exp13 12 -- +, -
      exp11 = chainLevel exp12 11 -- <<, >>, >>>
      exp10 = chainLevel exp11 10 -- <, <=, >, >=, in, instanceof
      exp9 = chainLevel exp10 9 -- ==, !=, ===, !==
      exp8 = chainLevel exp9 8 -- &
      exp7 = chainLevel exp8 7 -- bit xor
      exp6 = chainLevel exp7 6 -- bit or
      exp5 = chainLevel exp6 5 -- &&
      exp4 = chainLevel exp5 4 -- or
      exp3 = chainLevel exp4 3 -- ??
      exp2 = chainLevel exp3 2 -- Assignments
      exp1 = chainLevel exp2 1 -- comma
   in exp1

-- >>> parse (many varP) "x y z"
-- Right [Name "x",Name "y",Name "z"]
-- >>> parse varP "(x.y[1]).z"
-- Right (Dot (Var (Element (Var (Dot (Var (Name "x")) "y")) (Lit (NumberLiteral 1)))) "z")
varP :: Parser Var
varP = P.try (mkVar <$> prefixP <*> some indexP) <|> P.try (Name <$> nameP)
  where
    mkVar :: Expression -> [Expression -> Var] -> Var
    mkVar e l = foldr1 (\f p u -> p (Var (f u))) l e

    prefixP :: Parser Expression
    prefixP = parens expP <|> Var . Name <$> nameP

    indexP :: Parser (Expression -> Var)
    indexP =
      P.try (flip Dot <$> (P.string "." *> nameP))
        <|> P.try (flip Element <$> brackets expP)

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

-- >>> parse (many nameP) "x sfds $test_this he$_lo o9kay _ 9bad from but-not-this"
-- Right ["x","sfds","$test_this","he$_lo","o9kay","_"]
nameP :: Parser Name
nameP =
  wsP $ do
    varName <- (:) <$> (P.try P.letterChar <|> P.try (P.char '_') <|> P.try (P.char '$')) <*> many (P.try P.alphaNumChar <|> P.try (P.char '_') <|> P.try (P.char '$'))
    if varName `elem` reserved
      then fail "Reserved keyword"
      else return varName

-- >>> parse (many uopPrefixP) "- - ... --"
-- Right [MinusUop,MinusUop,Spread,DecPre]
-- >>> parse (many uopPrefixP) "~ \n- -    ! # ++ ... typeof void +++"
-- Right [BitNeg,MinusUop,MinusUop,Not]
uopPrefixP :: Parser UopPrefix
uopPrefixP =
  wsP $
    P.try (P.char '!' $> Not)
      <|> P.try (P.char '~' $> BitNeg)
      <|> P.try (P.string "typeof" $> TypeOf)
      <|> P.try (P.string "..." $> Spread)
      <|> P.try (P.string "--" $> DecPre)
      <|> P.try (P.string "++" $> IncPre)
      <|> P.try (P.char '+' $> PlusUop)
      <|> P.try (P.char '-' $> MinusUop)
      <|> P.try (P.string "void" $> Void)

-- >>> parse (many uopPrefixP) "++   ++ -- ++      --"
-- Right [IncPre,IncPre,DecPre,IncPre,DecPre]
uopPostfix :: Parser UopPostfix
uopPostfix =
  wsP $
    P.try (P.string "--" $> DecPost)
      <|> P.try (P.string "++" $> IncPost)

-- >>> parse (many bopP) "+ >= .."
-- Right [PlusBop,Ge]
-- >>> parse (many bopP) "|| >>= +   <= - //  \n== % * <<===> >"
-- Right [Or,RightShiftAssign,PlusBop,Le,MinusBop,Div,Div,Eq,Mod,Times,LeftShiftAssign,Eq,Gt,Gt]
bopP :: Parser Bop
bopP =
  P.try . wsP . asum $ -- TODO: do we need to move P.try?
    [ P.string ">>>" >> ((P.char '=' $> UnsignedRightShiftAssign) <|> pure UnsignedRightShift),
      P.string ">>" >> ((P.char '=' $> RightShiftAssign) <|> pure RightShift),
      P.string "<<" >> ((P.char '=' $> LeftShiftAssign) <|> pure LeftShift),
      P.string "**" >> ((P.char '=' $> ExpAssign) <|> pure Exp),
      P.string "==" >> ((P.char '=' $> EqStrict) <|> pure Eq),
      P.string "!=" >> ((P.char '=' $> NeqStrict) <|> pure Neq),
      P.string "&&" >> ((P.char '=' $> AndAssign) <|> pure And),
      P.string "||" >> ((P.char '=' $> OrAssign) <|> pure Or),
      P.string "??" >> ((P.char '=' $> NullishCoalescingAssign) <|> pure NullishCoalescing),
      P.string "+" >> ((P.char '=' $> PlusAssign) <|> pure PlusBop),
      P.string "-" >> ((P.char '=' $> MinusAssign) <|> pure MinusBop),
      P.string "*" >> ((P.char '=' $> TimesAssign) <|> pure Times),
      P.string "/" >> ((P.char '=' $> DivAssign) <|> pure Div),
      P.string "%" >> ((P.char '=' $> ModAssign) <|> pure Mod),
      P.string "&" >> ((P.char '=' $> BitAndAssign) <|> pure BitAnd),
      P.string "|" >> ((P.char '=' $> BitOrAssign) <|> pure BitOr),
      P.string "^" >> ((P.char '=' $> BitXorAssign) <|> pure BitXor),
      P.string "<" >> ((P.char '=' $> Le) <|> pure Lt),
      P.string ">" >> ((P.char '=' $> Ge) <|> pure Gt),
      P.string "=" $> Assign,
      P.string "in" $> In,
      P.string "instanceof" $> InstanceOf,
      P.string "," $> Comma
    ]

constAssignmentP :: Parser Statement
constAssignmentP =
  ConstAssignment
    <$> (stringP "const" *> varP)
    <*> (optional (stringP ":" *> typeP) *> stringP "=" *> expP <* optional (stringP ";"))

letAssignmentP :: Parser Statement
letAssignmentP =
  LetAssignment
    <$> (stringP "let" *> varP)
    <*> (optional (stringP ":" *> typeP) *> stringP "=" *> expP <* optional (stringP ";")) -- TODO: Support "let x;"

ifP :: Parser Statement -- TODO: Support else if and if with no else, ifs without braces
ifP =
  If
    <$> (stringP "if" *> parens expP)
    <*> (braces blockP <* stringP "else")
    <*> braces blockP

forP :: Parser Statement
forP =
  For
    <$> ( stringP "for"
            *> stringP "("
            *> (P.try constAssignmentP <|> P.try letAssignmentP)
        )
    <*> (expP <* stringP ";")
    <*> (expP <* stringP ")")
    <*> braces blockP

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

functionDeclarationP :: Parser Statement
functionDeclarationP = undefined

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

-- <|> whileP
-- <|> breakP
-- <|> continueP
-- <|> tryP
-- <|> returnP
-- <|> switchP
-- <|> labeledStatementP
-- <|> functionDeclarationP
-- <|> functionCallP
-- <|> emptyP

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
          (many literalP)
          "1 2\n 3"
          ~?= Right [NumberLiteral 1, NumberLiteral 2, NumberLiteral 3],
        parse
          (many literalP)
          "true false\n true"
          ~?= Right
            [ BooleanLiteral True,
              BooleanLiteral False,
              BooleanLiteral True
            ],
        parse
          (many literalP)
          "null null\n null"
          ~?= Right [NullLiteral, NullLiteral, NullLiteral],
        parse
          (many literalP)
          "undefined undefined\n undefined"
          ~?= Right [UndefinedLiteral, UndefinedLiteral, UndefinedLiteral],
        parse literalP "\"a\"" ~?= Right (StringLiteral "a"),
        parse literalP "\"a\\\"\"" ~?= Right (StringLiteral "a\\"),
        parse
          (many literalP)
          "\"a\"   \"b\""
          ~?= Right [StringLiteral "a", StringLiteral "b"],
        parse
          (many literalP)
          "\" a\"   \"b\""
          ~?= Right [StringLiteral " a", StringLiteral "b"]
      ]

test_exp :: Test
test_exp =
  "parsing expressions"
    ~: TestList
      [ parse (many varP) "x y z" ~?= Right [Name "x", Name "y", Name "z"],
        parse (many nameP) "x sfds _ from" ~?= Right ["x", "sfds", "_"],
        parse (many uopPrefixP) "- - ... --" ~?= Right [MinusUop, MinusUop, Spread, DecPre],
        parse (many bopP) "+ >= ||" ~?= Right [PlusBop, Ge, Or]
      ]

test_stat :: Test
test_stat =
  "parsing statements"
    ~: TestList
      [ parse statementP "const x = 3" ~?= Right (ConstAssignment (Name "x") (Lit (NumberLiteral 3))),
        parse statementP "if (x) { let y = undefined; } else { const y = null }"
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
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

test_all :: IO Counts
test_all =
  runTestTT $
    TestList [test_comb, test_literal, test_exp, test_stat, tParseFiles]

-- >>> test_all
-- Counts {cases = 22, tried = 22, errors = 0, failures = 0}

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
