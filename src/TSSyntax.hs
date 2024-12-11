module TSSyntax where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import TSType
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
import TSNumber
import Text.PrettyPrint qualified as PP

newtype Block = Block [Statement]
  deriving (Eq, Show)

instance Semigroup Block where
  (<>) :: Block -> Block -> Block
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty :: Block
  mempty = Block []

data Statement
  = ConstAssignment Var Expression -- const x: type = e
  | LetAssignment Var Expression -- let x: type = e
  | If Expression Block Block -- if (e) { s1 } else { s2 }
  | For Statement Expression Expression Block -- for (e1; e2; e3) { s }
  | While Expression Block -- while (e) { s }
  | Break
  | Continue
  | Try Block Expression Block -- try { s1 } catch (e) { s2 }
  | Return Expression
  | Switch Expression [(Expression, Block)] -- switch (e) { case e1: s1; ... }
  | LabeledStatement String Statement -- label: s
  | FunctionDeclaration Expression -- I was thinking of using this as a wrapper of FunctionExpression?
  -- I think we need some way to parse stuff like this (type annotations and optional parameters + unwrapping):
  -- type Obj = {
  --   x: number,
  --   y?: number
  -- };
  | FunctionCall Expression -- f(e1, ..., en)
  | Empty -- ';'
  deriving (Eq, Show)

data Expression
  = Var Var -- global variables x and table indexing
  | Lit Literal -- literal values
  | AnnotatedExpression TSType Expression -- e : type
  | UnaryOpPrefix UopPrefix Expression -- unary operators
  | UnaryOpPostfix Expression UopPostfix -- unary operators
  | BinaryOp Expression Bop Expression -- binary operators
  -- I am kind of confused about the [Expression], shouldn't it be statements?
  | FunctionExpression (Maybe String) [Expression] Block -- function (x, y) { s } and (x, y) => s
  | Array [Expression] -- [e1, ..., en]
  deriving (Eq, Show)

data Literal
  = NumberLiteral Number -- 1 or 5.0 or Infinity or -Infinity or NaN
  | StringLiteral String -- "abd" or 'abd' or `abd`? (support templates later?)
  | BooleanLiteral Bool -- true or false
  | NullLiteral -- null
  | UndefinedLiteral -- undefined
  | ObjectLiteral (Map String Expression) -- { x: e1, y: e2 }
  deriving (Eq, Show)

data UopPrefix
  = Not -- `!` :: a -> Bool
  | BitNeg -- `~` :: Int -> Int
  | TypeOf -- `typeof` :: a -> String
  | Spread -- `...` :: a -> [a]
  | DecPre -- `--` :: Int -> Int
  | IncPre -- `++` :: Int -> Int
  | PlusUop -- `+` :: Int -> Int
  | MinusUop -- `-` :: Int -> Int
  | Void -- `void` :: a -> Undefined // TODO: MAKE THESE TYPES MATCH the literals
  deriving (Eq, Show, Enum, Bounded)

data UopPostfix
  = DecPost -- `--` :: Int -> Int
  | IncPost -- `++` :: Int -> Int
  deriving (Eq, Show, Enum, Bounded)

data Bop
  = Assign -- `=` :: a -> a -> a
  | PlusBop -- `+`  :: Int -> Int -> Int
  | PlusAssign -- `+=` :: Int -> Int -> Int // TODO: Doesn't need to be Int (e.g. can be string) types wrong
  | MinusBop -- `-`  :: Int -> Int -> Int
  | MinusAssign -- `-=` :: Int -> Int -> Int
  | Times -- `*`  :: Int -> Int -> Int
  | TimesAssign -- `*=` :: Int -> Int -> Int
  | Div -- `/` :: Int -> Int -> Int   -- floor division
  | DivAssign -- `/=` :: Int -> Int -> Int
  | Mod -- `%`  :: Int -> Int -> Int   -- modulo
  | ModAssign -- `%=` :: Int -> Int -> Int
  | Exp -- `**` :: Int -> Int -> Int
  | ExpAssign -- `**=` :: Int -> Int -> Int
  | BitAnd -- `&` :: Int -> Int -> Int
  | BitAndAssign -- `&=` :: Int -> Int -> Int
  | BitOr -- `|` :: Int -> Int -> Int
  | BitOrAssign -- `|=` :: Int -> Int -> Int
  | BitXor -- `^` :: Int -> Int -> Int
  | BitXorAssign -- `^=` :: Int -> Int -> Int
  | LeftShift -- `<<` :: Int -> Int -> Int
  | LeftShiftAssign -- `<<=` :: Int -> Int -> Int
  | RightShift -- `>>` :: Int -> Int -> Int
  | RightShiftAssign -- `>>=` :: Int -> Int -> Int
  | UnsignedRightShift -- `>>>` :: Int -> Int -> Int
  | UnsignedRightShiftAssign -- `>>>=` :: Int -> Int -> Int
  | And -- `&&` :: Bool -> Bool -> Bool
  | AndAssign -- `&&=` :: Bool -> Bool -> Bool
  | Or -- `||` :: Bool -> Bool -> Bool
  | OrAssign -- `||=` :: Bool -> Bool -> Bool
  | NullishCoalescing -- `??` :: a -> a -> a
  | NullishCoalescingAssign -- `??=` :: a -> a -> a
  | Comma -- `,` :: a -> a -> a
  | Eq -- `==` :: a -> a -> Bool
  | Neq -- `!=` :: a -> a -> Bool
  | EqStrict -- `===` :: a -> a -> Bool
  | NeqStrict -- `!==` :: a -> a -> Bool
  | Gt -- `>`  :: a -> a -> Bool
  | Ge -- `>=` :: a -> a -> Bool
  | Lt -- `<`  :: a -> a -> Bool
  | Le -- `<=` :: a -> a -> Bool
  | In -- `in` :: a -> a -> Bool
  | InstanceOf -- `instanceof` :: a -> a -> Bool
  deriving (Eq, Show, Enum, Bounded)

type Name = String -- either the name of a variable or the name of a field

data Var
  = Name Name -- x, global variable
  | Dot Expression Name -- t.x
  | Element Expression Expression -- t[1]
  deriving (Eq, Show)

class PP a where
  pp :: a -> Doc

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: (PP a) => a -> String
pretty = PP.render . pp

-- | Compact version. Displays its argument without newlines.
oneLine :: (PP a) => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

instance PP Bool where
  pp :: Bool -> Doc
  pp True = PP.text "true"
  pp False = PP.text "false"

instance PP String where
  pp :: String -> Doc
  pp = PP.text

instance PP Int where
  pp :: Int -> Doc
  pp = PP.int

instance PP Double where
  pp :: Double -> Doc
  pp d = if d == fromInteger (round d)
    then PP.int (round d)
    else PP.double d

instance PP Number where
  pp :: Number -> Doc
  pp = PP.text . show

-- instance PP TableName where
--   pp :: TableName -> Doc
--   pp (TN x) = PP.text x

instance PP Var where
  pp :: Var -> Doc
  pp (Name n) = PP.text n
  pp (Dot (Var v) k) = pp v <> PP.text "." <> pp k
  pp (Dot t k) = PP.parens (pp t) <> PP.text "." <> pp k
  pp (Element (Var v) k) = pp v <> PP.brackets (pp k)
  pp (Element t k) = PP.parens (pp t) <> PP.brackets (pp k)

instance PP Literal where
  pp :: Literal -> Doc
  pp (NumberLiteral i) = pp i
  pp (StringLiteral s) = PP.text ("\"" <> s <> "\"")
  pp (BooleanLiteral b) = pp b
  pp NullLiteral = PP.text "null"
  pp UndefinedLiteral = PP.text "undefined"
  pp (ObjectLiteral m) = PP.braces (PP.sep (PP.punctuate PP.comma (map ppa (Map.toList m))))
    where
      ppa (s, v) = PP.text s <> (PP.colon <+> pp v)

instance PP TSType where
  pp :: TSType -> Doc
  pp TBoolean = PP.text "boolean"
  pp (TBooleanLiteral b) = if b then PP.text "true" else PP.text "false"
  pp TNumber = PP.text "number"
  pp (TNumberLiteral n) = pp n
  pp TString = PP.text "string"
  pp (TStringLiteral s) = pp s
  pp (TArray t) = pp t <> PP.text "[]"
  pp (TTuple t u) = PP.text "[" <> pp t <> PP.text ", " <> pp u <> PP.text "]"
  pp TBracket = PP.text "{}"
  pp TObject = PP.text "object"
  pp (TUserObject m) = undefined
  pp (TFunction ts t) = undefined
  pp TUnknown = PP.text "unknown"
  pp TAny = PP.text "any"
  pp TNever = PP.text "never"
  pp TVoid = PP.text "void"
  pp TNull = PP.text "null"
  pp TUndefined = PP.text "undefined"
  pp (TUnion ts) = PP.sep (PP.punctuate (PP.text " | ") (map pp ts))
  pp (TIntersection ts) = PP.sep (PP.punctuate (PP.text " & ") (map pp ts))

isBase :: Expression -> Bool
isBase Var {} = True
isBase Lit {} = True
isBase _ = False

hasSpace :: UopPrefix -> Bool
hasSpace TypeOf = True
hasSpace Void = True
hasSpace _ = False

missingSpaceBefore :: Bop -> Bool
missingSpaceBefore Comma = True
missingSpaceBefore _ = False

instance PP UopPrefix where
  pp :: UopPrefix -> Doc
  pp Not = PP.text "!"
  pp BitNeg = PP.text "~"
  pp TypeOf = PP.text "typeof"
  pp Spread = PP.text "..."
  pp DecPre = PP.text "--"
  pp IncPre = PP.text "++"
  pp PlusUop = PP.char '+'
  pp MinusUop = PP.char '-'
  pp Void = PP.text "void"

instance PP UopPostfix where
  pp :: UopPostfix -> Doc
  pp DecPost = PP.text "--"
  pp IncPost = PP.text "++"

instance PP Bop where
  pp :: Bop -> Doc
  pp Assign = PP.equals
  pp PlusBop = PP.char '+'
  pp PlusAssign = PP.text "+="
  pp MinusBop = PP.char '-'
  pp MinusAssign = PP.text "-="
  pp Times = PP.char '*'
  pp TimesAssign = PP.text "*="
  pp Div = PP.char '/'
  pp DivAssign = PP.text "/="
  pp Mod = PP.char '%'
  pp ModAssign = PP.text "%="
  pp Exp = PP.text "**"
  pp ExpAssign = PP.text "**="
  pp BitAnd = PP.char '&'
  pp BitAndAssign = PP.text "&="
  pp BitOr = PP.char '|'
  pp BitOrAssign = PP.text "|="
  pp BitXor = PP.char '^'
  pp BitXorAssign = PP.text "^="
  pp LeftShift = PP.text "<<"
  pp LeftShiftAssign = PP.text "<<="
  pp RightShift = PP.text ">>"
  pp RightShiftAssign = PP.text ">>="
  pp UnsignedRightShift = PP.text ">>>"
  pp UnsignedRightShiftAssign = PP.text ">>>="
  pp And = PP.text "&&"
  pp AndAssign = PP.text "&&="
  pp Or = PP.text "||"
  pp OrAssign = PP.text "||="
  pp NullishCoalescing = PP.text "??"
  pp NullishCoalescingAssign = PP.text "??="
  pp Comma = PP.char ','
  pp Eq = PP.text "=="
  pp Neq = PP.text "!="
  pp EqStrict = PP.text "==="
  pp NeqStrict = PP.text "!=="
  pp Gt = PP.char '>'
  pp Ge = PP.text ">="
  pp Lt = PP.char '<'
  pp Le = PP.text "<="
  pp In = PP.text "in"
  pp InstanceOf = PP.text "instanceof"

instance PP Expression where
  pp :: Expression -> Doc
  pp (Var v) = pp v
  pp (Lit l) = pp l
  pp (AnnotatedExpression e t) = pp e <> (PP.colon <+> pp t)
  pp (UnaryOpPrefix uop e) =
    if hasSpace uop
      then pp uop <+> if isBase e then pp e else PP.parens (pp e)
      else pp uop <> if isBase e then pp e else PP.parens (pp e)
  pp (UnaryOpPostfix e uop) = if isBase e then pp e else PP.parens (pp e) <> pp uop
  pp e@(BinaryOp _ _ _) = ppPrec 0 e
    where
      ppPrec n (BinaryOp e1 bop e2) =
        ppParens (level bop < n) $
          if missingSpaceBefore bop
            then ppPrec (level bop) e1 <> (pp bop <+> ppPrec (level bop + 1) e2)
            else ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
      ppPrec _ e' = pp e'
      ppParens b = if b then PP.parens else id
  pp (FunctionExpression _ _ _) = undefined -- TODO: finish this
  pp (Array es) = PP.brackets (PP.hcat (PP.punctuate PP.comma (map pp es)))

-- instance PP TableField where
--   pp :: TableField -> Doc
--   pp (FieldName name e) = pp name <+> PP.equals <+> pp e
--   pp (FieldKey e1 e2) = PP.brackets (pp e1) <+> PP.equals <+> pp e2

instance PP Block where
  pp :: Block -> Doc
  pp (Block [s]) = pp s
  pp (Block ss) = PP.vcat (map pp ss)

ppSS :: [Statement] -> Doc
ppSS ss = PP.vcat (map pp ss)

instance PP Statement where
  pp :: Statement -> Doc
  pp (ConstAssignment v e) = PP.text "const" <+> pp v <+> PP.equals <+> pp e
  pp (LetAssignment v e) = PP.text "let" <+> pp v <+> PP.equals <+> pp e
  pp (If guard b1 b2) =
    PP.hang (PP.text "if" <+> PP.parens (pp guard) <+> PP.char '{') 2 (pp b1)
      PP.$$ PP.nest 2 (PP.text "} else {" PP.$$ pp b2)
      PP.$$ PP.char '}'
  pp (For init guard update b) =
    PP.hang (PP.text "for" <+> PP.parens (pp init <> (PP.semi <+> (pp guard <> (PP.semi <+> pp update)))) <+> PP.char '{') 2 (pp b)
      PP.$$ PP.char '}'
  pp (While guard e) =
    PP.hang (PP.text "while" <+> PP.parens (pp guard) <+> PP.char '{') 2 (pp e)
      PP.$$ PP.char '}'
  pp Break = PP.text "break"
  pp Continue = PP.text "continue"
  pp (Try b1 e b2) =
    PP.hang (PP.text "try" <+> PP.char '{') 2 (pp b1)
      PP.$$ PP.text "catch" <+> PP.parens (pp e) <+> PP.char '{'
      PP.$$ pp b2
      PP.$$ PP.char '}'
  pp (Return e) = PP.text "return" <+> pp e -- TODO: support empty returns
  pp (Switch e cases) =
    PP.hang (PP.text "switch" <+> PP.parens (pp e) <+> PP.char '{') 2 (PP.vcat (map ppc cases))
      PP.$$ PP.char '}'
    where
      ppc (e, b) = PP.text "case" <+> (pp e <> (PP.char ':' <+> pp b))
  pp (LabeledStatement s st) = PP.text s <> (PP.char ':' <+> pp st)
  pp (FunctionDeclaration e) = undefined
  pp (FunctionCall e) = undefined
  pp Empty = PP.semi

level :: Bop -> Int
level b = case b of
  Comma -> 1

  Assign -> 2
  PlusAssign -> 2
  MinusAssign -> 2
  TimesAssign -> 2
  DivAssign -> 2
  ModAssign -> 2
  ExpAssign -> 2
  BitAndAssign -> 2
  BitOrAssign -> 2
  BitXorAssign -> 2
  LeftShiftAssign -> 2
  RightShiftAssign -> 2
  UnsignedRightShiftAssign -> 2
  AndAssign -> 2
  OrAssign -> 2
  NullishCoalescingAssign -> 2

  NullishCoalescing -> 3
  Or -> 4
  And -> 5
  BitOr -> 6
  BitXor -> 7
  BitAnd -> 8
  Eq -> 9
  Neq -> 9
  EqStrict -> 9
  NeqStrict -> 9
  Lt -> 10
  Le -> 10
  Gt -> 10
  Ge -> 10
  In -> 10
  InstanceOf -> 10

  LeftShift -> 11
  RightShift -> 11
  UnsignedRightShift -> 11

  PlusBop -> 12
  MinusBop -> 12

  Times -> 13
  Div -> 13
  Mod -> 13

  Exp -> 14

-- instance (PP a) => PP (Map Value a) where
--   pp :: (PP a) => Map Value a -> Doc
--   pp m = PP.braces (PP.vcat (map ppa (Map.toList m)))
--     where
--       ppa (StringVal s, v2) = PP.text s <+> PP.text "=" <+> pp v2
--       ppa (v1, v2) = PP.brackets (pp v1) <+> PP.text "=" <+> pp v2

-- instance (PP a) => PP (Map TableName a) where
--   pp :: (PP a) => Map TableName a -> Doc
--   pp m = PP.braces (PP.vcat (map ppa (Map.toList m)))
--     where
--       ppa (s, v2) = pp s <+> PP.text "=" <+> pp v2

-- >>> pretty ((Block [ConstAssignment (Name "num") (Lit (NumberLiteral 42)),ConstAssignment (Name "str") (Lit (StringLiteral "literal-string")),ConstAssignment (Name "boolTrue") (Lit (BooleanLiteral True)),ConstAssignment (Name "boolFalse") (Lit (BooleanLiteral False)),ConstAssignment (Name "arrOfNum") (Array [BinaryOp (BinaryOp (Lit (NumberLiteral 1)) Comma (Lit (NumberLiteral 2))) Comma (Lit (NumberLiteral 3))]),ConstAssignment (Name "arrOfStr") (Array [BinaryOp (BinaryOp (Lit (StringLiteral "a")) Comma (Lit (StringLiteral "b"))) Comma (Lit (StringLiteral "c"))]),ConstAssignment (Name "arrOfBool") (Array [BinaryOp (BinaryOp (Lit (BooleanLiteral True)) Comma (Lit (BooleanLiteral False))) Comma (Lit (BooleanLiteral True))]),ConstAssignment (Name "nullLiteral") (Lit NullLiteral),ConstAssignment (Name "decimal") (Lit (NumberLiteral 42)),ConstAssignment (Name "decimalFloat") (UnaryOpPrefix MinusUop (Lit (NumberLiteral 42.42))),ConstAssignment (Name "binary") (Lit (NumberLiteral 42)),ConstAssignment (Name "octal") (UnaryOpPrefix MinusUop (Lit (NumberLiteral 42))),ConstAssignment (Name "hexadecimal") (Lit (NumberLiteral 42)),ConstAssignment (Name "scientific") (BinaryOp (BinaryOp (BinaryOp (BinaryOp (UnaryOpPrefix MinusUop (Lit (NumberLiteral 42.0))) Times (BinaryOp (Lit (NumberLiteral 100)) Exp (Lit (NumberLiteral 2)))) LeftShift (Lit (NumberLiteral 4))) Div (Lit (NumberLiteral 4.2))) BitOr (Lit (NumberLiteral 93))),ConstAssignment (Name "scientificNegative") (Lit (NumberLiteral 0.42000000000000004)),ConstAssignment (Name "infinity") (Lit (NumberLiteral Infinity)),ConstAssignment (Name "negativeInfinity") (UnaryOpPrefix MinusUop (Lit (NumberLiteral Infinity))),ConstAssignment (Name "nan") (Lit (NumberLiteral NaN))]))

sampleVar :: IO ()
sampleVar = QC.sample' (arbitrary :: Gen Var) >>= mapM_ (print . pp)

sampleExp :: IO ()
sampleExp = QC.sample' (arbitrary :: Gen Expression) >>= mapM_ (print . pp)

sampleStat :: IO ()
sampleStat = QC.sample' (arbitrary :: Gen Statement) >>= mapM_ (print . pp)

quickCheckN :: (QC.Testable prop) => Int -> prop -> IO ()
quickCheckN n = QC.quickCheckWith $ QC.stdArgs {QC.maxSuccess = n, QC.maxSize = 100}

-- | Generate a small set of names for generated tests. These names are guaranteed to not include
-- reserved words
genName :: Gen Name
genName = QC.elements ["x", "X", "y", "x0", "X0", "xy", "XY"]

-- | Generate a string literal, being careful about the characters that it may contain
genStringLit :: Gen String
genStringLit = escape <$> QC.listOf (QC.elements stringLitChars)
  where
    -- escape special characters appearing in the string,
    escape :: String -> String
    escape = foldr Char.showLitChar ""
    -- generate strings containing printable characters or spaces, but not including '\"'
    stringLitChars :: [Char]
    stringLitChars = filter (\c -> c /= '\"' && (Char.isSpace c || Char.isPrint c)) ['\NUL' .. '~']

-- | Generate a size-controlled global variable or table field
genVar :: Int -> Gen Var
-- genVar 0 = Name <$> genName
-- genVar n =
--   QC.frequency
--     [ (1, Name <$> genName),
--       (n, Dot <$> genExp n' <*> genName)
--       (n, Proj <$> genExp n' <*> genExp n')
--     ]
--   where
--     n' = n `div` 2
genVar _ = undefined

-- | Generate a size-controlled expression
genExp :: Int -> Gen Expression
-- genExp 0 = QC.oneof [Var <$> genVar 0, Val <$> arbitrary]
-- genExp n =
--   QC.frequency
--     [ (1, Var <$> genVar n),
--       (1, Val <$> arbitrary),
--       (n, Op1 <$> arbitrary <*> genExp n'),
--       (n, Op2 <$> genExp n' <*> arbitrary <*> genExp n'),
--       (n', TableConst <$> genTableFields n')
--     ]
--   where
--     n' = n `div` 2
genExp _ = undefined

-- -- | Generate a list of fields in a table constructor expression.
-- -- We limit the size of the table to avoid size blow up.
-- genTableFields :: Int -> Gen [TableField]
-- genTableFields n = do
--   len <- QC.elements [0 .. 3]
--   take len <$> QC.infiniteListOf (genTableField n)

-- genTableField :: Int -> Gen TableField
-- genTableField n =
--   QC.oneof
--     [ FieldName <$> genName <*> genExp n',
--       FieldKey <$> genExp n' <*> genExp n'
--     ]
--   where
--     n' = n `div` 2

-- | Generate a size-controlled statement
genStatement :: Int -> Gen Statement
-- genStatement n | n <= 1 = QC.oneof [Assign <$> genVar 0 <*> genExp 0, return Empty]
-- genStatement n =
--   QC.frequency
--     [ (1, Assign <$> genVar n' <*> genExp n'),
--       (1, return Empty),
--       (n, If <$> genExp n' <*> genBlock n' <*> genBlock n'),
--       -- generate loops half as frequently as if statements
--       (n', While <$> genExp n' <*> genBlock n'),
--       (n', Repeat <$> genBlock n' <*> genExp n')
--     ]
--   where
--     n' = n `div` 2
genStatement _ = undefined

genBlock :: Int -> Gen Block
genBlock n = Block <$> genStmts n
  where
    genStmts 0 = pure []
    genStmts n =
      QC.frequency
        [ (1, return []),
          (n, (:) <$> genStatement n' <*> genStmts n')
        ]
      where
        n' = n `div` 2

-- instance Arbitrary TableName where
--   arbitrary :: Gen TableName
--   arbitrary = QC.elements [TN "_", TN "_G", TN "_x", TN "_t1"]

instance Arbitrary Var where
  arbitrary :: Gen Var
  arbitrary = QC.sized genVar
  shrink :: Var -> [Var]
  -- shrink (Name n) = []
  -- shrink (Proj e1 e2) =
  --   [Proj e1' e2 | e1' <- shrink e1]
  --     ++ [Proj e1 e2' | e2' <- shrink e2]
  -- shrink (Dot e n) = [Dot e' n | e' <- shrink e]
  shrink _ = undefined

instance Arbitrary Statement where
  arbitrary :: Gen Statement
  arbitrary = QC.sized genStatement
  shrink :: Statement -> [Statement]
  -- shrink (Assign v e) =
  --   [Assign v' e | v' <- shrink v]
  --     ++ [Assign v e' | e' <- shrink e]
  -- shrink (If e b1 b2) =
  --   first b1
  --     ++ first b2
  --     ++ [If e' b1 b2 | e' <- shrink e]
  --     ++ [If e b1' b2 | b1' <- shrink b1]
  --     ++ [If e b1 b2' | b2' <- shrink b2]
  -- shrink (While e b) =
  --   first b
  --     ++ [While e' b | e' <- shrink e]
  --     ++ [While e b' | b' <- shrink b]
  -- shrink (Repeat b e) =
  --   first b
  --     ++ [Repeat b' e | b' <- shrink b]
  --     ++ [Repeat b e' | e' <- shrink e]
  -- shrink Empty = []
  shrink _ = undefined

-- | access the first statement in a block, if one exists
first :: Block -> [Statement]
first (Block []) = []
first (Block (x : _)) = [x]

-- -- | access expressions in a table field
-- getExp :: TableField -> [Expression]
-- getExp (FieldName _ e) = [e]
-- getExp (FieldKey e1 e2) = [e1, e2]

-- instance Arbitrary TableField where
--   arbitrary :: Gen TableField
--   arbitrary = QC.sized genTableField
--   shrink :: TableField -> [TableField]
--   shrink (FieldName n e1) = [FieldName n e1' | e1' <- shrink e1]
--   shrink (FieldKey e1 e2) =
--     [FieldKey e1' e2 | e1' <- shrink e1]
--       ++ [FieldKey e1 e2' | e2' <- shrink e2]

instance Arbitrary Block where
  arbitrary :: Gen Block
  arbitrary = QC.sized genBlock
  shrink :: Block -> [Block]
  shrink (Block ss) = [Block ss' | ss' <- shrink ss]

instance Arbitrary Expression where
  arbitrary :: Gen Expression
  arbitrary = QC.sized genExp

  shrink :: Expression -> [Expression]
  -- shrink (Val v) = Val <$> shrink v
  -- shrink (Var v) = Var <$> shrink v
  -- shrink (Op1 o e) = e : [Op1 o e' | e' <- shrink e]
  -- shrink (Op2 e1 o e2) =
  --   [Op2 e1' o e2 | e1' <- shrink e1]
  --     ++ [Op2 e1 o e2' | e2' <- shrink e2]
  --     ++ [e1, e2]
  -- shrink (TableConst fs) = concatMap getExp fs ++ (TableConst <$> shrink fs)
  shrink _ = undefined

instance Arbitrary UopPrefix where
  arbitrary :: Gen UopPrefix
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary UopPostfix where
  arbitrary :: Gen UopPostfix
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Bop where
  arbitrary :: Gen Bop
  arbitrary = QC.arbitraryBoundedEnum

shrinkStringLit :: String -> [String]
shrinkStringLit s = filter (/= '\"') <$> shrink s

instance Arbitrary Literal where
  arbitrary :: Gen Literal
  arbitrary =
    QC.oneof
      [ NumberLiteral <$> arbitrary,
        BooleanLiteral <$> arbitrary,
        pure NullLiteral,
        pure UndefinedLiteral,
        StringLiteral <$> genStringLit
        -- TODO: add Objects
      ]

  shrink :: Literal -> [Literal]
  shrink (NumberLiteral n) = NumberLiteral <$> shrink n
  shrink (BooleanLiteral b) = BooleanLiteral <$> shrink b
  shrink NullLiteral = []
  shrink UndefinedLiteral = []
  shrink (StringLiteral s) = StringLiteral <$> shrinkStringLit s
  shrink (ObjectLiteral m) = ObjectLiteral <$> shrink m
