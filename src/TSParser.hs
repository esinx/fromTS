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
wsP p = p <* P.space

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
-- Right [1,0,23,-23,1,5,-5,5,0.5,0.5,-0.5,-0.5,-124,10,10,100,100,-1,10,5.0e-5,2.0e-2,-10,0,1,14,-4,-505,1,-64195,49374,-Infinity,Infinity,NaN,5]

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
-- TODO: Support Object literals
literalP :: Parser Literal
literalP =
  tryChoice
    [ NumberLiteral <$> numberValP,
      BooleanLiteral <$> boolValP,
      NullLiteral <$ stringIsoP "null",
      UndefinedLiteral <$ stringIsoP "undefined",
      StringLiteral <$> stringValP
    ]

--------------------------------------------------------------------------------

-- | Non-Primitive, Non-Greedy
arrayTypeP :: Parser TSType
arrayTypeP =
  tryChoice
    [ stringP "Array" *> abrackets typeP,
      TArray <$> (typeP <* brackets (pure ()))
    ]

-- | Non-Primitive, Non-Greedy
tupleP :: Parser [TSType]
tupleP = brackets (typeP `P.sepBy` stringP ",")

-- | Non-Primitive, Non-Greedy
unionP :: Parser [TSType]
unionP = typeP `P.sepBy` stringP "|"

-- | Non-Primitive, Non-Greedy
intersectionP :: Parser [TSType]
intersectionP = typeP `P.sepBy` stringP "&"

-- | Primitive, Greedy
-- TODO: Add bracket, object, UserObject, function
typeP :: Parser TSType
typeP =
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
        TArray <$> arrayTypeP,
        TTuple <$> tupleP,
        TUnknown <$ stringIsoP "unknown",
        TAny <$ stringIsoP "any",
        TNever <$ stringIsoP "never",
        TVoid <$ stringIsoP "void",
        TNull <$ stringIsoP "null",
        TUndefined <$ stringIsoP "undefined",
        TUnion <$> unionP, -- TODO: FIX UNION AND INTERSECTION PRIORITY/PRECEDENCE
        TIntersection <$> intersectionP
      ]

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
        stringIso "in" $> In,
        P.string "," $> Comma
      ]

-- >>> parse (many bopP) "+ >= .."
-- Right [PlusBop,Ge]
-- >>> parse (many bopP) "|| >>= +   <= - //  \n== % * <<===> >"
-- Right [Or,RightShiftAssign,PlusBop,Le,MinusBop,Div,Div,Eq,Mod,Times,LeftShiftAssign,Eq,Gt,Gt]

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
-- Right (Dot (Var (Element (Var (Dot (Var (Name "x")) "y")) (Lit (NumberLiteral 1)))) "z")

-- | Non-Primitive, Greedy
baseExpP :: Parser Expression
baseExpP =
  tryChoice
    [ Lit <$> literalP,
      Array <$> brackets (expP `P.sepBy` stringP ","),
      Var <$> varP,
      parens expP
    ]

-- | Non-Primitive, Greedy
annotatedExpP :: Parser Expression
annotatedExpP = do
  e <- baseExpP
  me <- optional (stringP ":" *> typeP)
  case me of
    Just t -> return (AnnotatedExpression t e)
    Nothing -> return e

-- | Non-Primitive, Greedy
prefixExpP :: Parser Expression
prefixExpP =
  tryChoice
    [ UnaryOpPrefix <$> uopPrefixP <*> prefixExpP,
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
      exp1 = chainLevel exp2 1 -- comma
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
        Just t -> AnnotatedExpression t exp
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
        Just t -> AnnotatedExpression t exp
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
-- TODO: remove function call statement and treat as expression
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
          e <- parens expP
          cblock <- braces blockP
          fblock <- P.option (Block []) (stringIsoP "finally" *> braces blockP)
          return (Just e, cblock, fblock),
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
functionDeclarationP :: Parser Statement
functionDeclarationP = undefined

-- | Non-Primitive, Greedy
functionCallP :: Parser Statement
functionCallP = undefined

-- | Non-Primitive, Greedy
emptyP :: Parser Statement
emptyP = Empty <$ stringP ";"

-- | Primitive, Greedy
statementP :: Parser Statement
statementP =
  tryChoice
    [ constAssignmentP <* optional (stringP ";"),
      letAssignmentP <* optional (stringP ";"),
      ifP <* optional (stringP ";"),
      forP <* optional (stringP ";"),
      whileP <* optional (stringP ";"),
      breakP <* optional (stringP ";"),
      continueP <* optional (stringP ";"),
      tryP <* optional (stringP ";"),
      returnP <* optional (stringP ";"),
      AnyExpression <$> expP <* optional (stringP ";"),
      -- functionDeclarationP <* optional (stringP ";"),
      -- functionCallP <* optional (stringP ";"),
      emptyP
    ]

-- | Non-Primitive, Greedy
blockP :: Parser Block
blockP = Block <$> many statementP

--------------------------------------------------------------------------------

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
      [] -- TODO: FILL IN
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
                [ ( Var (Name "x"),
                    Block [LetAssignment (Name "y") (Lit UndefinedLiteral)]
                  )
                ]
                (Block [ConstAssignment (Name "y") (Lit NullLiteral)])
            )
      ]

-- >>> runTestTT test_stat
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

test_all :: IO Counts
test_all =
  runTestTT $
    TestList [test_comb, test_literal, test_exp, test_stat, tParseFiles]

-- >>> test_all
-- Counts {cases = 22, tried = 22, errors = 0, failures = 0}

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
