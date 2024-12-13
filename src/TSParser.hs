module TSParser where

import Control.Applicative
import Control.Monad (guard, mapM_)
import Data.Char qualified as Char
import Data.Functor
import Data.List (foldl')
import Data.List.NonEmpty qualified as NE
import Data.Map (Map, fromList)
import Data.Map qualified as Map
import Data.Set (singleton)
import Data.Void (Void)
import System.IO qualified as IO
import System.IO.Error qualified as IO
import TSGen
import TSNumber
import TSSyntax
import TSType
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error qualified as P
import Text.Megaparsec.Error.Builder qualified as P
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

-- IMPORTANT: The parsing functions below are labeled with two tags:
-- 1. Primitive vs. Non-Primitive
--      Primitive functions will never consume any input on failure
--      Non-primitive functions can consume input even if they fail
-- 2. Greedy vs. Non-Greedy
--      Greedy functions will consume as much whitespace (and comments)
--        as possible after the parsed value
--      Non-greedy functions will consume only the required token
--
-- Some functions labelled with Non-Primitive are technically Primitive
-- (specifically those involving tryChoice), but they should still conceptually
-- be treated as Non-Primitive

-- | First arg: custom error component, second arg: input stream type
type Parser = P.Parsec Void String

-- | First arg: input stream type, second arg: custom error component
type ParserError = P.ParseErrorBundle String Void

-- | Second argument of P.parse is for error messages
parse :: Parser a -> String -> Either ParserError a
parse = flip P.parse ""

-- | Consumes whitespace and comments (single-line and multi-line)
spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    P.space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- | Applies a parser and then consumes trailing whitespace/comments
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Returns Primitive parser, maintains greediness
filterPMessage :: (a -> Bool) -> Parser a -> String -> Parser a
filterPMessage f p s = P.try $ do
  x <- p
  if f x
    then return x
    else fail s

-- | Returns Primitive parser, maintains greediness
filterP :: (a -> Bool) -> Parser a -> Parser a
filterP f p = filterPMessage f p "Predicate not satisfied"

-- | Returns Primitive parser, maintains greediness
tryChoice :: (Functor f, Foldable f, P.MonadParsec e s m) => f (m a) -> m a
tryChoice = P.choice . fmap P.try

-- | Returns Primitive parser, maintains greediness
tryOptional :: (Alternative m, P.MonadParsec e s m) => m a -> m (Maybe a)
tryOptional = optional . P.try

-- | Returns Primitive parser, maintains greediness
tryOption :: (Alternative m, P.MonadParsec e s m) => a -> m a -> m a
tryOption b = P.option b . P.try

-- | Returns Primitive parser, maintains greediness
tryMany :: (Alternative m, P.MonadParsec e s m) => m a -> m [a]
tryMany = many . P.try

-- | Returns Primitive parser, maintains greediness
trySome :: (Alternative m, P.MonadParsec e s m) => m a -> m [a]
trySome = some . P.try

-- | Parses one or more occurrences of @p@ separated by binary operator
-- parser @pop@.  Returns a value produced by a /left/ associative application
-- of all functions returned by @pop@.
-- See the end of the `Parsers` lecture for explanation of this operator.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` pop = foldl comb <$> p <*> rest
  where
    comb x (op, y) = x `op` y
    rest = tryMany ((,) <$> pop <*> p)

--------------------------------------------------------------------------------

-- | Returns Greedy parser, maintains primitivity
wsP :: Parser a -> Parser a
wsP = lexeme

test_wsP :: Test
test_wsP =
  TestList
    [ parse (wsP P.letterChar) "a" ~?= Right 'a',
      parse (many (wsP P.letterChar)) "a b \n   \t c" ~?= Right "abc"
    ]

-- >>> runTestTT test_wsP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

-- | Primitive, Greedy
stringP :: String -> Parser ()
stringP s = wsP (P.string s) $> ()

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

-- | Primitive, Greedy
constP :: String -> a -> Parser a
constP s x = wsP (P.string s) $> x

test_constP :: Test
test_constP =
  TestList
    [ parse (constP "&" 'a') "&  " ~?= Right 'a',
      parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]

-- >>> runTestTT test_constP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

-- | Primitive, Non-Greedy
nameCharP :: Parser Char
nameCharP = P.alphaNumChar <|> P.char '_' <|> P.char '$'

-- | Primitive, Non-Greedy
stringIso :: String -> Parser String
stringIso s = P.try $ P.string s <* P.notFollowedBy nameCharP

-- | Primitive, Greedy
stringIsoP :: String -> Parser ()
stringIsoP s = wsP (stringIso s) $> ()

-- | Primitive, Greedy
constIsoP :: String -> a -> Parser a
constIsoP s x = wsP (stringIso s) $> x

-- | Returns Greedy parser, maintains primitivity
parens :: Parser a -> Parser a
parens = P.between (stringP "(") (stringP ")")

-- | Returns Greedy parser, maintains primitivity
braces :: Parser a -> Parser a
braces = P.between (stringP "{") (stringP "}")

-- | Returns Greedy parser, maintains primitivity
brackets :: Parser a -> Parser a
brackets = P.between (stringP "[") (stringP "]")

-- | Returns Greedy parser, maintains primitivity
abrackets :: Parser a -> Parser a
abrackets = P.between (stringP "<") (stringP ">")

-- >>> parse (many (parens (constP "1" 1))) "(1) (  1)   (1 )"
-- Right [1,1,1]
-- >>> parse (many (braces (constP "1" 1))) "{1} {  1}   {1 }"
-- Right [1,1,1]
-- >>> parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]"
-- Right [1,1,1]

--------------------------------------------------------------------------------

-- | Helper function to parse a negative number
-- | Returns Non-Primitive parser, maintains greediness
negateNum :: (Num a) => Parser a -> Parser a
negateNum p = negate <$> (P.char '-' *> p)

-- | Helper function to parse number with any sign
-- | Returns Non-Primitive parser, maintains greediness
signed :: (Num a) => Parser a -> Parser a
signed p = tryChoice [negateNum p, p]

-- | Helper function to convert a string of digits to an integer
convertBase :: Int -> String -> Int
convertBase base = foldl' (\acc c -> acc * base + Char.digitToInt c) 0

-- | Parse a positive integer (base 10)
-- | Non-Primitive, Non-Greedy
intP :: Parser Int
intP = convertBase 10 <$> some P.digitChar

-- | Parse a positive binary integer
-- | Non-Primitive, Non-Greedy
binaryP :: Parser Int
binaryP = convertBase 2 <$> (P.string' "0b" *> some P.binDigitChar)

-- | Parse a positive octal integer
-- | Non-Primitive, Non-Greedy
octalP :: Parser Int
octalP = convertBase 8 <$> (P.string' "0o" *> some P.octDigitChar)

-- | Parse a positive hexadecimal integer (case insensitive)
-- | Non-Primitive, Non-Greedy
hexP :: Parser Int
hexP = convertBase 16 <$> (P.string' "0x" *> some P.hexDigitChar)

-- | Parse a positive double
-- | Non-Primitive, Non-Greedy
doubleP :: Parser Double
doubleP = do
  integerPart <- many P.digitChar
  _ <- P.char '.'
  fractionalPart <- many P.digitChar
  let combined = integerPart ++ "." ++ fractionalPart
  let doubleOptional = case (integerPart, fractionalPart) of
        ("", "") -> Nothing
        ("", b) -> readMaybe ("0." ++ b)
        (a, "") -> readMaybe (a ++ ".0")
        (a, b) -> readMaybe combined
  case doubleOptional of
    Just x -> return x
    Nothing -> fail $ "Can't parse '" ++ combined ++ "' as a double"

-- | Parse a positive scientific number
-- | Non-Primitive, Non-Greedy
scientificP :: Parser Double
scientificP =
  (\base exp -> base * 10 ^^ exp)
    <$> (P.try doubleP <|> fromIntegral <$> intP)
    <* P.string' "e"
    <*> signed intP

-- | Parse a TS number
-- | Primitive, Greedy
numberValP :: Parser Number
numberValP =
  wsP $
    tryChoice
      [ NaN <$ (stringIso "NaN" <|> stringIso "-NaN"),
        NInfinity <$ stringIso "-Infinity",
        Infinity <$ stringIso "Infinity",
        Double <$> signed scientificP,
        Double <$> signed doubleP,
        Double . fromIntegral <$> signed binaryP,
        Double . fromIntegral <$> signed octalP,
        Double . fromIntegral <$> signed hexP,
        Double . fromIntegral <$> signed intP
      ]

-- >>> parse (many (wsP numberValP)) "1 0 23 -23 1.0 5.0 -5.0 5. .5 0.5 -.5 -0.5 -124. 1e1 1E1 10e1 10.e1 -.10e1 0.1e2 5E-5 2e-2 -1.0e1 -0.e2 0b1 0B1110 -0o4 -0O771 0x1 -0XFaC3 0XC0DE -Infinity Infinity NaN 5"
-- Right [Double 1.0,Double 0.0,Double 23.0,Double (-23.0),Double 1.0,Double 5.0,Double (-5.0),Double 5.0,Double 0.5,Double 0.5,Double (-0.5),Double (-0.5),Double (-124.0),Double 10.0,Double 10.0,Double 100.0,Double 100.0,Double (-1.0),Double 10.0,Double 5.0e-5,Double 2.0e-2,Double (-10.0),Double (-0.0),Double 1.0,Double 14.0,Double (-4.0),Double (-505.0),Double 1.0,Double (-64195.0),Double 49374.0,NInfinity,Infinity,NaN,Double 5.0]

-- | Primitive, Greedy
boolValP :: Parser Bool
boolValP = tryChoice [True <$ stringIsoP "true", False <$ stringIsoP "false"]

-- >>> parse (many boolValP) "true false\n true"
-- Right [True,False,True]

-- | Primitive, Greedy
-- TODO: Support nested strings and escaping
stringValP :: Parser String
stringValP =
  tryChoice
    [ P.between (P.char '"') (stringP "\"") (many (P.satisfy (/= '"'))),
      P.between (P.char '\'') (stringP "'") (many (P.satisfy (/= '\''))),
      P.between (P.char '`') (stringP "`") (many (P.satisfy (/= '`')))
    ]

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

-- | Primitive, Greedy
propertyP :: Parser (String, Expression)
propertyP = P.try $ do
  key <- stringValP <|> nameP
  stringP ":"
  val <- expP
  return (key, val)

objectValP :: Parser (Map String Expression)
objectValP = fromList <$> braces (propertyP `P.sepEndBy` stringP ",")

-- | Primitive, Greedy
literalP :: Parser Literal
literalP =
  tryChoice
    [ NumberLiteral <$> numberValP,
      BooleanLiteral <$> boolValP,
      NullLiteral <$ stringIsoP "null",
      UndefinedLiteral <$ stringIsoP "undefined",
      StringLiteral <$> stringValP,
      ObjectLiteral <$> objectValP
    ]

--------------------------------------------------------------------------------

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

-- | Primitive, Greedy
nameP :: Parser Name
nameP =
  wsP $
    filterPMessage
      (`notElem` reserved)
      ((:) <$> (P.letterChar <|> P.char '_' <|> P.char '$') <*> many nameCharP)
      "Cannot use reserved word as variable name"

-- >>> parse (many nameP) "x sfds $test_this he$_lo o9kay _ 9bad from but-not-this"
-- Right ["x","sfds","$test_this","he$_lo","o9kay","_"]

--------------------------------------------------------------------------------

-- | Primitive, Greedy
typePropertyP :: Parser (String, TSType)
typePropertyP = P.try $ do
  key <- stringValP <|> nameP
  isOpt <- optional (stringP "?")
  stringP ":"
  tp <- typeP
  let finalTp = case isOpt of
        Just _ -> TUnion [tp, TUndefined]
        Nothing -> tp
  return (key, finalTp)

-- | Primitive, Greedy
-- TODO: stop accepting two fields on same line (not trivial)
objectTypeP :: Parser TSType
objectTypeP = TUserObject . fromList <$> braces (typePropertyP `P.sepEndBy` optional (stringP ";" <|> stringP ","))

-- | Primitive, Greedy
baseTypeP :: Parser TSType
baseTypeP =
  wsP $
    tryChoice
      [ TBooleanLiteral <$> boolValP,
        TBoolean <$ stringIsoP "boolean",
        TNumberLiteral <$> signed scientificP,
        TNumberLiteral <$> signed doubleP,
        TNumberLiteral . fromIntegral <$> signed binaryP,
        TNumberLiteral . fromIntegral <$> signed octalP,
        TNumberLiteral . fromIntegral <$> signed hexP,
        TNumberLiteral . fromIntegral <$> signed intP,
        TNumber <$ stringIsoP "number",
        TStringLiteral <$> stringValP,
        TString <$ stringIsoP "string",
        TUnknown <$ stringIsoP "unknown",
        TAny <$ stringIsoP "any",
        TNever <$ stringIsoP "never",
        TVoid <$ stringIsoP "void",
        TNull <$ stringIsoP "null",
        TUndefined <$ stringIsoP "undefined",
        TObject <$ stringIsoP "object",
        TTuple <$> brackets (typeP `P.sepEndBy` stringP ","),
        -- TArray <$ stringIsoP "Array" *> abrackets typeP,
        TBracket <$ braces (pure ()),
        objectTypeP,
        TTypeAlias <$> nameP
      ]

-- | Non-Primitive, Greedy
parseArrays :: TSType -> Parser TSType
parseArrays base =
  ( do
      brackets (pure ())
      parseArrays (TArray base)
  )
    <|> return base

-- | Non-Primitive, Greedy
primaryTypeP :: Parser TSType
primaryTypeP = wsP $ do
  base <- P.try baseTypeP <|> parens typeP
  parseArrays base

-- | Primitive, Greedy
intersectionTypeP :: Parser TSType
intersectionTypeP = do
  intersections <- primaryTypeP `P.sepBy1` stringP "&"
  return $
    case intersections of
      [single] -> single
      multiple -> TIntersection multiple

-- | Primitive, Greedy
typeP :: Parser TSType
typeP = do
  unions <- intersectionTypeP `P.sepBy1` stringP "|"
  return $
    case unions of
      [single] -> single
      multiple -> TUnion multiple

--------------------------------------------------------------------------------

-- | Primitive, Greedy
uopPrefixP :: Parser UopPrefix
uopPrefixP =
  tryChoice
    [ BitNeg <$ stringP "~",
      Not <$ stringP "!",
      Spread <$ stringP "...",
      DecPre <$ stringP "--",
      IncPre <$ stringP "++",
      PlusUop <$ stringP "+",
      MinusUop <$ stringP "-",
      TypeOf <$ stringIsoP "typeof",
      Void <$ stringIsoP "void"
    ]

-- >>> parse (many uopPrefixP) "- - ... --"
-- Right [MinusUop,MinusUop,Spread,DecPre]
-- >>> parse (many uopPrefixP) "~ \n- -    ! # ++ ... typeof void +++"
-- Right [BitNeg,MinusUop,MinusUop,Not]

-- | Primitive, Greedy
uopPostfix :: Parser UopPostfix
uopPostfix =
  tryChoice
    [ DecPost <$ stringP "--",
      IncPost <$ stringP "++"
    ]

-- >>> parse (many uopPrefixP) "++   ++ -- ++      --"
-- Right [IncPre,IncPre,DecPre,IncPre,DecPre]

-- | Primitive, Greedy
bopP :: Parser Bop
bopP =
  wsP $
    tryChoice
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
        stringIso "instanceof" $> InstanceOf,
        stringIso "in" $> In
      ]

-- >>> parse (many bopP) "+ >= .."
-- Right [PlusBop,Ge]
-- >>> parse (many bopP) "|| >>= +   <= - / /  \n== % * <<===> >"
-- Right [Or,RightShiftAssign,PlusBop,Le,MinusBop,Div,Div,Eq,Mod,Times,LeftShiftAssign,Eq,Gt,Gt]

-- | Primitive, Greedy
varP :: Parser Var
varP = tryChoice [mkVar <$> prefixP <*> some indexP, Name <$> nameP]
  where
    mkVar :: Expression -> [Expression -> Var] -> Var
    mkVar e l = foldr1 (\f p u -> p (Var (f u))) l e

    prefixP :: Parser Expression
    prefixP = tryChoice [parens expP, Var . Name <$> nameP]

    indexP :: Parser (Expression -> Var)
    indexP =
      tryChoice
        [ flip Dot <$> (P.string "." *> nameP),
          flip Element <$> brackets expP
        ]

-- >>> parse (many varP) "x y z"
-- Right [Name "x",Name "y",Name "z"]
-- >>> parse varP "(x.y[1]).z"
-- Right (Dot (Var (Element (Var (Dot (Var (Name "x")) "y")) (Lit (NumberLiteral (Double 1.0))))) "z")

-- | Non-Primitive, Greedy
baseExpP :: Parser Expression
baseExpP =
  tryChoice
    [ Array <$> brackets (expP `P.sepEndBy` stringP ","),
      Lit <$> literalP,
      Var <$> varP,
      parens expP
    ]

-- | Non-Primitive, Greedy
annotatedExpP :: Parser Expression
annotatedExpP = do
  e <- baseExpP
  me <- optional (stringP ":" *> typeP)
  case me of
    Just t -> return (AnnotatedExpression (TSTypeWrapper t) e)
    Nothing -> return e

-- | Non-Primitive, Greedy
prefixExpP :: Parser Expression
prefixExpP =
  tryChoice
    [ -- Lit . NumberLiteral <$> (NInfinity <$ stringIso "-Infinity"),
      UnaryOpPrefix <$> uopPrefixP <*> prefixExpP,
      annotatedExpP
    ]

-- | Primitive, Greedy
postfixExpP :: Parser Expression
postfixExpP = P.try $ do
  e <- prefixExpP
  me <- optional uopPostfix
  case me of
    Just op -> return (UnaryOpPostfix e op)
    Nothing -> return e

-- | Parse an operator at a specified precedence level
-- | Returns Primitive, Greedy parser
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip BinaryOp <$> filterP (\x -> level x == l) bopP

chainLevel :: Parser Expression -> Int -> Parser Expression
chainLevel sub l = sub `chainl1` opAtLevel l

-- | Primitive, Greedy
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
      exp1 = chainLevel exp2 1 -- none
   in exp1

--------------------------------------------------------------------------------

-- | Non-Primitive, Greedy
constAssignmentP :: Parser Statement
constAssignmentP = do
  _ <- stringIsoP "const"
  name <- nameP -- TODO: Support destructuring
  expType <- tryOptional (stringP ":" *> typeP)
  _ <- stringP "="
  exp <- expP
  let finalExpr = case expType of
        Nothing -> exp
        Just t -> AnnotatedExpression (TSTypeWrapper t) exp
  return (ConstAssignment (Name name) finalExpr)

-- | Non-Primitive, Greedy
-- TODO: Support "let x;" and "let x: number;"
letAssignmentP :: Parser Statement
letAssignmentP = do
  _ <- stringIsoP "let"
  name <- nameP -- TODO: Support destructuring
  expType <- tryOptional (stringP ":" *> typeP)
  _ <- stringP "="
  exp <- expP
  let finalExpr = case expType of
        Nothing -> exp
        Just t -> AnnotatedExpression (TSTypeWrapper t) exp
  return (LetAssignment (Name name) finalExpr)

-- | Primitive, Greedy
blockOrStmtP :: Parser Block
blockOrStmtP =
  tryChoice
    [ braces blockP,
      Block . (: []) <$> statementP
    ]

-- | Non-Primitive, Greedy
ifP :: Parser Statement
ifP = do
  -- Parse initial 'if (...)'
  stringIsoP "if"
  cond <- parens expP
  ifBlock <- blockOrStmtP

  -- Parse zero or more 'else if (...)' clauses
  elseIfs <- tryMany $ do
    stringIsoP "else"
    stringIsoP "if"
    c <- parens expP
    b <- blockOrStmtP
    return (c, b)

  -- Parse optional 'else'
  elseBlock <- P.option (Block []) $ do
    stringIsoP "else"
    blockOrStmtP

  return (If ((cond, ifBlock) : elseIfs) elseBlock)

-- | Primitive, Greedy
-- TODO: support `for (;;) {}` (e.g. empty expressions/statements)
forAssignmentP :: Parser Statement
forAssignmentP =
  tryChoice [constAssignmentP, letAssignmentP, AnyExpression <$> expP]

-- | Non-Primitive, Greedy
forP :: Parser Statement
forP =
  For
    <$> ( stringIsoP "for"
            *> stringP "("
            *> forAssignmentP
            <* stringP ";"
        )
    <*> (expP <* stringP ";")
    <*> (expP <* stringP ")")
    <*> blockOrStmtP

-- | Non-Primitive, Greedy
whileP :: Parser Statement
whileP = While <$> (stringIsoP "while" *> parens expP) <*> blockOrStmtP

-- | Non-Primitive, Greedy
breakP :: Parser Statement
breakP = Break <$ stringIsoP "break"

-- | Non-Primitive, Greedy
continueP :: Parser Statement
continueP = Continue <$ stringIsoP "continue"

-- | Non-Primitive, Greedy
tryP :: Parser Statement
tryP = do
  _ <- stringIsoP "try"
  b1 <- braces blockP

  (mbE, b2, b3) <-
    tryChoice
      [ -- Attempt to parse catch + optional finally
        do
          _ <- stringIsoP "catch"
          e <- P.optional (parens expP)
          cblock <- braces blockP
          fblock <- P.option (Block []) (stringIsoP "finally" *> braces blockP)
          return (e, cblock, fblock),
        -- If no catch, try finally
        do
          fblock <- stringIsoP "finally" *> braces blockP
          return (Nothing, Block [], fblock)
      ]

  return (Try b1 mbE b2 b3)

-- | Non-Primitive, Greedy
returnP :: Parser Statement
returnP = do
  _ <- stringIsoP "return"
  e <- optional expP
  return (Return e)

-- | Non-Primitive, Greedy
typeAliasP :: Parser Statement
typeAliasP = do
  stringIsoP "type"
  n <- nameP
  stringP "="
  TypeAlias n . TSTypeWrapper <$> typeP

-- | Non-Primitive, Greedy
interfaceP :: Parser Statement
interfaceP = do
  stringIsoP "interface"
  n <- nameP
  InterfaceDeclaration n . TSTypeWrapper <$> objectTypeP

-- | Non-Primitive, Greedy
emptyP :: Parser Statement
emptyP = Empty <$ stringP ";"

-- | Primitive, Greedy
statementP :: Parser Statement
statementP =
  tryChoice
    [ typeAliasP <* optional (stringP ";"),
      interfaceP <* optional (stringP ";"),
      constAssignmentP <* optional (stringP ";"),
      letAssignmentP <* optional (stringP ";"),
      ifP <* optional (stringP ";"),
      forP <* optional (stringP ";"),
      whileP <* optional (stringP ";"),
      breakP <* optional (stringP ";"),
      continueP <* optional (stringP ";"),
      tryP <* optional (stringP ";"),
      returnP <* optional (stringP ";"),
      AnyExpression <$> expP <* optional (stringP ";"),
      emptyP
    ]

-- | Non-Primitive, Greedy
blockP :: Parser Block
blockP = Block <$> (spaceConsumer *> many statementP)

--------------------------------------------------------------------------------

parseTSExp :: String -> Either ParserError Expression
parseTSExp = parse expP

parseTSStat :: String -> Either ParserError Statement
parseTSStat = parse statementP

parseTSSource :: String -> Either ParserError Block
parseTSSource = parse (const <$> blockP <*> P.eof)

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

wVariables :: Block
wVariables = Block [ConstAssignment (Name "num") (BinaryOp (Lit (NumberLiteral (Double 1.0))) PlusBop (Lit (NumberLiteral (Double 1.0)))), ConstAssignment (Name "str") (BinaryOp (Lit (StringLiteral "literal-string")) PlusBop (Lit (StringLiteral "concatenated"))), ConstAssignment (Name "boolTrue") (BinaryOp (Lit (BooleanLiteral True)) Or (Lit (BooleanLiteral False))), ConstAssignment (Name "boolFalse") (BinaryOp (Lit (BooleanLiteral True)) And (Lit (BooleanLiteral False)))]

wObject :: Block
wObject = Block [ConstAssignment (Name "obj") (Lit (ObjectLiteral (fromList [("key", Lit (StringLiteral "value")), ("nested", Lit (ObjectLiteral (fromList [("key", Lit (StringLiteral "value"))])))])))]

wConstLiterals :: Block
wConstLiterals = Block [ConstAssignment (Name "num") (Lit (NumberLiteral (Double 42.0))), ConstAssignment (Name "str") (Lit (StringLiteral "literal-string")), ConstAssignment (Name "boolTrue") (Lit (BooleanLiteral True)), ConstAssignment (Name "boolFalse") (Lit (BooleanLiteral False))]

wBfs :: Block
wBfs = Block [ConstAssignment (Name "graph") (AnnotatedExpression (TSTypeWrapper (TArray (TArray TNumber))) (Array [Array [Lit (NumberLiteral (Double 1.0)), Lit (NumberLiteral (Double 2.0))], Array [Lit (NumberLiteral (Double 3.0)), Lit (NumberLiteral (Double 4.0))], Array [Lit (NumberLiteral (Double 5.0))], Array [], Array [Lit (NumberLiteral (Double 5.0))], Array []])), ConstAssignment (Name "queue") (AnnotatedExpression (TSTypeWrapper (TArray TNumber)) (Array [])), LetAssignment (Name "head") (Lit (NumberLiteral (Double 0.0))), LetAssignment (Name "tail") (Lit (NumberLiteral (Double 0.0))), ConstAssignment (Name "startNode") (Lit (NumberLiteral (Double 0.0))), AnyExpression (BinaryOp (Var (Element (Var (Name "queue")) (Var (Name "tail")))) Assign (Var (Name "startNode"))), AnyExpression (UnaryOpPostfix (Var (Name "tail")) IncPost), ConstAssignment (Name "visited") (AnnotatedExpression (TSTypeWrapper (TArray TBoolean)) (Array [Lit (BooleanLiteral False), Lit (BooleanLiteral False), Lit (BooleanLiteral False), Lit (BooleanLiteral False), Lit (BooleanLiteral False), Lit (BooleanLiteral False)])), AnyExpression (BinaryOp (Var (Element (Var (Name "visited")) (Var (Name "startNode")))) Assign (Lit (BooleanLiteral True))), While (BinaryOp (Var (Name "head")) Neq (Var (Name "tail"))) (Block [ConstAssignment (Name "currentNode") (Var (Element (Var (Name "queue")) (Var (Name "head")))), AnyExpression (UnaryOpPostfix (Var (Name "head")) IncPost), ConstAssignment (Name "neighbors") (Var (Element (Var (Name "graph")) (Var (Name "currentNode")))), For (LetAssignment (Name "i") (Lit (NumberLiteral (Double 0.0)))) (BinaryOp (Var (Name "i")) Lt (Var (Dot (Var (Name "neighbors")) "length"))) (UnaryOpPostfix (Var (Name "i")) IncPost) (Block [ConstAssignment (Name "neighbor") (Var (Element (Var (Name "neighbors")) (Var (Name "i")))), If [(UnaryOpPrefix Not (Var (Element (Var (Name "visited")) (Var (Name "neighbor")))), Block [AnyExpression (BinaryOp (Var (Element (Var (Name "visited")) (Var (Name "neighbor")))) Assign (Lit (BooleanLiteral True))), AnyExpression (BinaryOp (Var (Element (Var (Name "queue")) (Var (Name "tail")))) Assign (Var (Name "neighbor"))), AnyExpression (UnaryOpPostfix (Var (Name "tail")) IncPost)])] (Block [])])])]

wLiteralsSimple :: Block
wLiteralsSimple = Block [ConstAssignment (Name "num") (Lit (NumberLiteral (Double 42.0))), ConstAssignment (Name "str") (Lit (StringLiteral "literal-string")), ConstAssignment (Name "boolTrue") (AnnotatedExpression (TSTypeWrapper (TBooleanLiteral False)) (Lit (BooleanLiteral True))), ConstAssignment (Name "boolFalse") (AnnotatedExpression (TSTypeWrapper TBoolean) (Lit (BooleanLiteral False))), ConstAssignment (Name "arrOfNum") (Array [Lit (NumberLiteral (Double 1.0)), Lit (NumberLiteral (Double 2.0)), Lit (NumberLiteral (Double 3.0))]), ConstAssignment (Name "arrOfStr") (Array [Lit (StringLiteral "a"), Lit (StringLiteral "b"), Lit (StringLiteral "c")]), ConstAssignment (Name "arrOfBool") (Array [Lit (BooleanLiteral True), Lit (BooleanLiteral False), Lit (BooleanLiteral True)]), ConstAssignment (Name "nullLiteral") (Lit NullLiteral), ConstAssignment (Name "decimal") (AnnotatedExpression (TSTypeWrapper TNumber) (Lit (NumberLiteral (Double 42.0)))), ConstAssignment (Name "decimalFloat") (UnaryOpPrefix MinusUop (Lit (NumberLiteral (Double 42.42)))), ConstAssignment (Name "binary") (Lit (NumberLiteral (Double 42.0))), ConstAssignment (Name "octal") (AnnotatedExpression (TSTypeWrapper TString) (UnaryOpPrefix MinusUop (Lit (NumberLiteral (Double 42.0))))), ConstAssignment (Name "hexadecimal") (Lit (NumberLiteral (Double 42.0))), ConstAssignment (Name "scientific") (BinaryOp (BinaryOp (BinaryOp (BinaryOp (UnaryOpPrefix MinusUop (Lit (NumberLiteral (Double 42.0)))) Times (BinaryOp (Lit (NumberLiteral (Double 100.0))) Exp (Lit (NumberLiteral (Double 2.0))))) LeftShift (Lit (NumberLiteral (Double 4.0)))) Div (Lit (NumberLiteral (Double 4.2)))) BitOr (Lit (NumberLiteral (Double 93.0)))), ConstAssignment (Name "scientificNegative") (Lit (NumberLiteral (Double 0.42000000000000004))), ConstAssignment (Name "infinity") (Lit (NumberLiteral Infinity)), ConstAssignment (Name "negativeInfinity") (UnaryOpPrefix MinusUop (Lit (NumberLiteral Infinity))), ConstAssignment (Name "nan") (Lit (NumberLiteral NaN)), For (LetAssignment (Name "x") (Lit (NumberLiteral (Double 0.0)))) (BinaryOp (Var (Name "x")) Lt (Lit (NumberLiteral (Double 10.0)))) (UnaryOpPostfix (Var (Name "x")) IncPost) (Block [AnyExpression (BinaryOp (Var (Name "i")) Assign (Var (Name "x")))]), If [(Lit (BooleanLiteral True), Block [LetAssignment (Name "val") (Lit (StringLiteral "5 == 6"))]), (BinaryOp (BinaryOp (Lit (NumberLiteral (Double 5.0))) MinusBop (Lit (NumberLiteral (Double 7.0)))) Lt (Lit (NumberLiteral (Double 21.0))), Block [AnyExpression (BinaryOp (Var (Name "val2")) Assign (Lit (BooleanLiteral True)))])] (Block []), Try (Block [AnyExpression (BinaryOp (Lit (NumberLiteral (Double 5.0))) PlusBop (Lit (NumberLiteral (Double 6.0))))]) (Just (AnnotatedExpression (TSTypeWrapper TAny) (Var (Name "a")))) (Block [ConstAssignment (Name "names") (Array [Lit (StringLiteral "a"), Lit (StringLiteral "b"), Lit (StringLiteral "c")])]) (Block [])]

wLiterals :: Block
wLiterals = Block [ConstAssignment (Name "num") (Lit (NumberLiteral (Double 42.0))), ConstAssignment (Name "str") (Lit (StringLiteral "literal-string")), ConstAssignment (Name "boolTrue") (AnnotatedExpression (TSTypeWrapper (TBooleanLiteral False)) (Lit (BooleanLiteral True))), ConstAssignment (Name "boolFalse") (AnnotatedExpression (TSTypeWrapper TBoolean) (Lit (BooleanLiteral False))), ConstAssignment (Name "constObj") (AnnotatedExpression (TSTypeWrapper (TTuple [TNumber, TBoolean])) (Lit (ObjectLiteral (fromList [("key", Lit (StringLiteral "value"))])))), ConstAssignment (Name "obj") (Lit (ObjectLiteral (fromList [("key", Lit (StringLiteral "value")), ("key2", Lit (NumberLiteral (Double 42.0)))]))), ConstAssignment (Name "arrOfNum") (Array [Lit (NumberLiteral (Double 1.0)), Lit (NumberLiteral (Double 2.0)), Lit (NumberLiteral (Double 3.0))]), ConstAssignment (Name "arrOfStr") (Array [Lit (StringLiteral "a"), Lit (StringLiteral "b"), Lit (StringLiteral "c")]), ConstAssignment (Name "arrOfBool") (Array [Lit (BooleanLiteral True), Lit (BooleanLiteral False), Lit (BooleanLiteral True)]), ConstAssignment (Name "arrOfObj") (Array [Lit (ObjectLiteral (fromList [("key", Lit (StringLiteral "value"))])), Lit (ObjectLiteral (fromList [("key", Lit (StringLiteral "value"))]))]), ConstAssignment (Name "arrOfArr") (Array [Array [Lit (NumberLiteral (Double 1.0)), Lit (NumberLiteral (Double 2.0)), Lit (NumberLiteral (Double 3.0))], Array [Lit (StringLiteral "a"), Lit (StringLiteral "b"), Lit (StringLiteral "c")]]), ConstAssignment (Name "arrOfMixed") (Array [Lit (NumberLiteral (Double 1.0)), Lit (StringLiteral "a"), Lit (BooleanLiteral True), Lit (ObjectLiteral (fromList [("key", Lit (StringLiteral "value"))])), Array [Lit (NumberLiteral (Double 1.0)), Lit (NumberLiteral (Double 2.0)), Lit (NumberLiteral (Double 3.0))]]), ConstAssignment (Name "nullLiteral") (Lit NullLiteral), ConstAssignment (Name "decimal") (AnnotatedExpression (TSTypeWrapper TNumber) (Lit (NumberLiteral (Double 42.0)))), ConstAssignment (Name "decimalFloat") (UnaryOpPrefix MinusUop (Lit (NumberLiteral (Double 42.42)))), ConstAssignment (Name "binary") (Lit (NumberLiteral (Double 42.0))), ConstAssignment (Name "octal") (AnnotatedExpression (TSTypeWrapper TString) (UnaryOpPrefix MinusUop (Lit (NumberLiteral (Double 42.0))))), ConstAssignment (Name "hexadecimal") (Lit (NumberLiteral (Double 42.0))), ConstAssignment (Name "scientific") (BinaryOp (BinaryOp (BinaryOp (BinaryOp (UnaryOpPrefix MinusUop (Lit (NumberLiteral (Double 42.0)))) Times (BinaryOp (Lit (NumberLiteral (Double 100.0))) Exp (Lit (NumberLiteral (Double 2.0))))) LeftShift (Lit (NumberLiteral (Double 4.0)))) Div (Lit (NumberLiteral (Double 4.2)))) BitOr (Lit (NumberLiteral (Double 93.0)))), ConstAssignment (Name "scientificNegative") (Lit (NumberLiteral (Double 0.42000000000000004))), ConstAssignment (Name "infinity") (Lit (NumberLiteral Infinity)), ConstAssignment (Name "negativeInfinity") (UnaryOpPrefix MinusUop (Lit (NumberLiteral Infinity))), ConstAssignment (Name "nan") (Lit (NumberLiteral NaN)), TypeAlias "Test1" (TSTypeWrapper (TUserObject (fromList [("test", TIntersection [TTuple [TNumberLiteral 5.0, TNumberLiteral 3.0], TTuple [TStringLiteral "test", TArray (TNumberLiteral 4.0), TString]])]))), InterfaceDeclaration "Test2" (TSTypeWrapper (TUserObject (fromList [("test", TNumberLiteral 5.0)]))), ConstAssignment (Name "test1") (AnnotatedExpression (TSTypeWrapper (TTypeAlias "Test2")) (Lit (ObjectLiteral (fromList [("test", Lit (NumberLiteral (Double 5.0)))])))), For (LetAssignment (Name "x") (Lit (NumberLiteral (Double 0.0)))) (BinaryOp (Var (Name "x")) Lt (Lit (NumberLiteral (Double 10.0)))) (UnaryOpPostfix (Var (Name "x")) IncPost) (Block [AnyExpression (BinaryOp (Var (Name "i")) Assign (Var (Name "x")))]), If [(Lit (BooleanLiteral True), Block [LetAssignment (Name "val") (Lit (StringLiteral "5 == 6"))]), (BinaryOp (BinaryOp (Lit (NumberLiteral (Double 5.0))) MinusBop (Lit (NumberLiteral (Double 7.0)))) Lt (Lit (NumberLiteral (Double 21.0))), Block [AnyExpression (BinaryOp (Var (Name "val2")) Assign (Lit (BooleanLiteral True)))])] (Block [AnyExpression (BinaryOp (Var (Name "test1")) Assign (Lit (BooleanLiteral True)))]), If [(Lit (BooleanLiteral False), Block [LetAssignment (Name "val") (Lit (StringLiteral "5 != 6"))]), (BinaryOp (BinaryOp (Lit (NumberLiteral (Double 5.0))) MinusBop (Lit (NumberLiteral (Double 7.0)))) Gt (Lit (NumberLiteral (Double 21.0))), Block [AnyExpression (BinaryOp (Var (Name "val3")) Assign (Lit (BooleanLiteral False)))])] (Block []), If [(BinaryOp (Lit (NumberLiteral (Double 5.0))) Eq (Lit (NumberLiteral (Double 6.0))), Block [LetAssignment (Name "val") (Lit (StringLiteral "5 != 6"))])] (Block [LetAssignment (Name "val") (Lit (StringLiteral "5 == 6"))]), While (Lit (BooleanLiteral True)) (Block [LetAssignment (Name "val") (Lit (StringLiteral "5 == 6"))]), Try (Block [AnyExpression (BinaryOp (Lit (NumberLiteral (Double 5.0))) PlusBop (Lit (NumberLiteral (Double 6.0))))]) (Just (AnnotatedExpression (TSTypeWrapper TAny) (Var (Name "a")))) (Block [ConstAssignment (Name "names") (Array [Lit (StringLiteral "a"), Lit (StringLiteral "b"), Lit (StringLiteral "c")])]) (Block []), Try (Block [AnyExpression (BinaryOp (Lit (NumberLiteral (Double 5.0))) PlusBop (Lit (NumberLiteral (Double 6.0))))]) Nothing (Block [ConstAssignment (Name "names") (Array [Lit (StringLiteral "a"), Lit (StringLiteral "b"), Lit (StringLiteral "c")])]) (Block [LetAssignment (Name "val") (Lit (NumberLiteral (Double 7.0)))]), Try (Block [AnyExpression (BinaryOp (Lit (NumberLiteral (Double 5.0))) PlusBop (Lit (NumberLiteral (Double 6.0))))]) Nothing (Block []) (Block [LetAssignment (Name "val") (Lit (NumberLiteral (Double 7.0)))]), TypeAlias "MyType" (TSTypeWrapper (TUnion [TNumber, TString, TBoolean])), ConstAssignment (Name "test") (AnnotatedExpression (TSTypeWrapper (TTypeAlias "MyType")) (Lit (NumberLiteral (Double 5.0)))), TypeAlias "MyType2" (TSTypeWrapper (TUserObject (fromList [("field1", TNumber), ("field2", TUnion [TString, TUndefined]), ("field3", TUnion [TBoolean, TTypeAlias "MyType"]), ("field4", TArray (TUnion [TNumber, TUnion [TTypeAlias "MyType2", TTypeAlias "MyType"]])), ("test field", TNumberLiteral 5.0)]))), InterfaceDeclaration "MyType3" (TSTypeWrapper (TUserObject (fromList [("field1", TNumber), ("field2", TString), ("field3", TUnion [TUnion [TBoolean, TTypeAlias "MyType"], TUndefined]), ("field4", TUnion [TUnion [TNumber, TArray (TUnion [TTypeAlias "MyType2", TTypeAlias "MyType"])], TUndefined])])))]

tParseFiles :: Test
tParseFiles =
  "parse files"
    ~: TestList
      [ "variables.ts" ~: p "test/variables.ts" wVariables,
        "object.ts" ~: p "test/object.ts" wObject,
        "const-literals.ts" ~: p "test/const-literals.ts" wConstLiterals,
        "bfs.ts" ~: p "test/bfs.ts" wBfs,
        "literals-simple.ts" ~: p "test/literals-simple.ts" wLiteralsSimple,
        "literals.ts" ~: p "test/literals.ts" wLiterals
      ]
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
          ~?= Right [NumberLiteral (Double 1), NumberLiteral (Double 2), NumberLiteral (Double 3)],
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
      [ parse statementP "const x = 3" ~?= Right (ConstAssignment (Name "x") (Lit (NumberLiteral (Double 3)))),
        parse statementP "if (x) { let y = undefined; } else { const y = null }"
          ~?= Right
            ( If
                [ ( Var (Name "x"),
                    Block [LetAssignment (Name "y") (Lit UndefinedLiteral)]
                  )
                ]
                (Block [ConstAssignment (Name "y") (Lit NullLiteral)])
            )
      ]

-- >>> runTestTT test_stat
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
