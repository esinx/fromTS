module TSSyntax where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import TSNumber
import TSType
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
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
  = AnyExpression Expression
  | ConstAssignment Var Expression -- const x: type = e
  | LetAssignment Var Expression -- let x: type = e
  | If [(Expression, Block)] Block -- if (e) { s1 } else { s2 }
  | For Statement Expression Expression Block -- for (s; e1; e2) { s }
  | While Expression Block -- while (e) { s }
  | Break
  | Continue
  | Try Block (Maybe Expression) Block Block -- try { s1 } catch (e) { s2 } finally { s3 }
  | Return (Maybe Expression)
  | FunctionDeclaration Expression -- I was thinking of using this as a wrapper of FunctionExpression?
  | FunctionCall Expression -- f(e1, ..., en)
  | TypeAlias Name TSType
  | InterfaceDeclaration Name TSType
  | Empty
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
  | StringLiteral String -- "abd" or 'abd' or `abd` TODO: support templates
  | BooleanLiteral Bool -- true or false
  | NullLiteral -- null
  | UndefinedLiteral -- undefined
  | ObjectLiteral (Map String Expression) -- { x: e1, y: e2 }
  deriving (Eq, Show)

data UopPrefix
  = Not -- `!`
  | BitNeg -- `~`
  | TypeOf -- `typeof`
  | Spread -- `...`
  | DecPre -- `--`
  | IncPre -- `++`
  | PlusUop -- `+`
  | MinusUop -- `-`
  | Void -- `void`
  deriving (Eq, Show, Enum, Bounded)

data UopPostfix
  = DecPost -- `--`
  | IncPost -- `++`
  deriving (Eq, Show, Enum, Bounded)

data Bop
  = Assign -- `=`
  | PlusBop -- `+`
  | PlusAssign -- `+=`
  | MinusBop -- `-`
  | MinusAssign -- `-=`
  | Times -- `*`
  | TimesAssign -- `*=`
  | Div -- `/`
  | DivAssign -- `/=`
  | Mod -- `%`
  | ModAssign -- `%=`
  | Exp -- `**`
  | ExpAssign -- `**=`
  | BitAnd -- `&`
  | BitAndAssign -- `&=`
  | BitOr -- `|`
  | BitOrAssign -- `|=`
  | BitXor -- `^`
  | BitXorAssign -- `^=`
  | LeftShift -- `<<`
  | LeftShiftAssign -- `<<=`
  | RightShift -- `>>`
  | RightShiftAssign -- `>>=`
  | UnsignedRightShift -- `>>>`
  | UnsignedRightShiftAssign -- `>>>=`
  | And -- `&&`
  | AndAssign -- `&&=`
  | Or -- `||`
  | OrAssign -- `||=`
  | NullishCoalescing -- `??`
  | NullishCoalescingAssign -- `??=`
  | Comma -- `,`
  | Eq -- `==`
  | Neq -- `!=`
  | EqStrict -- `===`
  | NeqStrict -- `!==`
  | Gt -- `>`
  | Ge -- `>=`
  | Lt -- `<`
  | Le -- `<=`
  | In -- `in`
  | InstanceOf -- `instanceof`
  deriving (Eq, Show, Enum, Bounded)

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
  pp d =
    if d == fromInteger (round d)
      then PP.int (round d)
      else PP.double d

instance PP Number where
  pp :: Number -> Doc
  pp = PP.text . show

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
  pp (TTuple ts) = PP.brackets (PP.hcat (PP.punctuate PP.comma (map pp ts)))
  pp TBracket = PP.text "{}"
  pp TObject = PP.text "object"
  pp (TTypeAlias n) = pp n
  pp (TUserObject m) = PP.braces (PP.sep (PP.punctuate PP.comma (map ppa (Map.toList m))))
    where
      ppa (s, v) = PP.text s <> (PP.colon <+> pp v)
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
  pp e@(BinaryOp {}) = ppPrec 0 e
    where
      ppPrec n (BinaryOp e1 bop e2) =
        ppParens (level bop < n) $
          if missingSpaceBefore bop
            then ppPrec (level bop) e1 <> (pp bop <+> ppPrec (level bop + 1) e2)
            else ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
      ppPrec _ e' = pp e'
      ppParens b = if b then PP.parens else id
  pp (FunctionExpression {}) = undefined -- TODO: finish this
  pp (Array es) = PP.brackets (PP.hcat (PP.punctuate PP.comma (map pp es)))

-- instance PP TableField where
--   pp :: TableField -> Doc
--   pp (FieldName name e) = pp name <+> PP.equals <+> pp e
--   pp (FieldKey e1 e2) = PP.brackets (pp e1) <+> PP.equals <+> pp e2

instance PP Block where
  pp :: Block -> Doc
  pp (Block [s]) = pp s
  pp (Block ss) = PP.vcat (map pp ss)

isEmptyBlock :: Block -> Bool
isEmptyBlock b = pp b == PP.empty

ppSS :: [Statement] -> Doc
ppSS ss = PP.vcat (map pp ss)

instance PP Statement where
  pp :: Statement -> Doc
  pp (AnyExpression e) = pp e
  pp (ConstAssignment v e) = PP.text "const" <+> pp v <+> PP.equals <+> pp e
  pp (LetAssignment v e) = PP.text "let" <+> pp v <+> PP.equals <+> pp e
  pp (If [] elseBlock) =
    PP.text "if (false) {} else {"
      PP.$$ PP.nest 2 (pp elseBlock)
      PP.$$ PP.char '}'
  pp (If ((cond, block) : rest) elseBlock) =
    ppIfChain (cond, block) rest elseBlock
    where
      ppIfChain :: (Expression, Block) -> [(Expression, Block)] -> Block -> Doc
      ppIfChain (c, b) [] elseBlk =
        -- No more conditions, just print if and optional else
        PP.hang (PP.text "if" <+> PP.parens (pp c) <+> PP.char '{') 2 (pp b)
          PP.$$ PP.char '}'
            <> ( if not (isEmptyBlock elseBlk)
                   then
                     PP.text " else {"
                       PP.$$ PP.nest 2 (pp elseBlk)
                       PP.$$ PP.char '}'
                   else PP.empty
               )
      ppIfChain (c, b) ((c2, b2) : xs) elseBlk =
        -- Still have more conditions, print `if` then chain `else if`s
        PP.hang (PP.text "if" <+> PP.parens (pp c) <+> PP.char '{') 2 (pp b)
          PP.$$ PP.char '}'
            <> PP.text " else"
            <> PP.hang (PP.text "if" <+> PP.parens (pp c2) <+> PP.char '{') 2 (pp b2)
          PP.$$ PP.char '}'
            <> ppElseIfChain xs elseBlk

      ppElseIfChain :: [(Expression, Block)] -> Block -> Doc
      ppElseIfChain [] elseBlk =
        if not (isEmptyBlock elseBlk)
          then
            PP.text " else {"
              PP.$$ PP.nest 2 (pp elseBlk)
              PP.$$ PP.char '}'
          else PP.empty
      ppElseIfChain ((c, b) : xs) elseBlk =
        PP.text " else"
          <> PP.hang (PP.text "if" <+> PP.parens (pp c) <+> PP.char '{') 2 (pp b)
          PP.$$ PP.char '}'
            <> ppElseIfChain xs elseBlk
  pp (For init guard update b) =
    PP.hang (PP.text "for" <+> PP.parens (pp init <> (PP.semi <+> (pp guard <> (PP.semi <+> pp update)))) <+> PP.char '{') 2 (pp b)
      PP.$$ PP.char '}'
  pp (While guard e) =
    PP.hang (PP.text "while" <+> PP.parens (pp guard) <+> PP.char '{') 2 (pp e)
      PP.$$ PP.char '}'
  pp Break = PP.text "break"
  pp Continue = PP.text "continue"
  pp (Try b1 mbE b2 b3) =
    PP.hang (PP.text "try {") 2 (pp b1)
      PP.$$ PP.char '}'
        <> ( case mbE of
               Nothing -> PP.empty
               Just e ->
                 PP.text " catch ("
                   <> pp e
                   <> PP.text ") {"
                   PP.$$ PP.nest 2 (pp b2)
                   PP.$$ PP.char '}'
           )
        <> ( if isEmptyBlock b3
               then PP.empty
               else
                 PP.text " finally {"
                   PP.$$ PP.nest 2 (pp b3)
                   PP.$$ PP.char '}'
           )
  pp (Return Nothing) = PP.text "return"
  pp (Return (Just e)) = PP.text "return" <+> pp e
  pp (FunctionDeclaration e) = undefined
  pp (FunctionCall e) = undefined
  pp (TypeAlias n t) = PP.text "type" <+> pp n <+> PP.equals <+> pp t
  pp (InterfaceDeclaration n t) = PP.text "interface" <+> pp n <+> PP.equals <+> pp t
  pp Empty = PP.empty

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

-- >>> pretty ((Block [ConstAssignment (Name "num") (Lit (NumberLiteral 42)),ConstAssignment (Name "str") (Lit (StringLiteral "literal-string")),ConstAssignment (Name "boolTrue") (Lit (BooleanLiteral True)),ConstAssignment (Name "boolFalse") (Lit (BooleanLiteral False)),ConstAssignment (Name "arrOfNum") (Array [BinaryOp (BinaryOp (Lit (NumberLiteral 1)) Comma (Lit (NumberLiteral 2))) Comma (Lit (NumberLiteral 3))]),ConstAssignment (Name "arrOfStr") (Array [BinaryOp (BinaryOp (Lit (StringLiteral "a")) Comma (Lit (StringLiteral "b"))) Comma (Lit (StringLiteral "c"))]),ConstAssignment (Name "arrOfBool") (Array [BinaryOp (BinaryOp (Lit (BooleanLiteral True)) Comma (Lit (BooleanLiteral False))) Comma (Lit (BooleanLiteral True))]),ConstAssignment (Name "nullLiteral") (Lit NullLiteral),ConstAssignment (Name "decimal") (Lit (NumberLiteral 42)),ConstAssignment (Name "decimalFloat") (UnaryOpPrefix MinusUop (Lit (NumberLiteral 42.42))),ConstAssignment (Name "binary") (Lit (NumberLiteral 42)),ConstAssignment (Name "octal") (UnaryOpPrefix MinusUop (Lit (NumberLiteral 42))),ConstAssignment (Name "hexadecimal") (Lit (NumberLiteral 42)),ConstAssignment (Name "scientific") (BinaryOp (BinaryOp (BinaryOp (BinaryOp (UnaryOpPrefix MinusUop (Lit (NumberLiteral 42.0))) Times (BinaryOp (Lit (NumberLiteral 100)) Exp (Lit (NumberLiteral 2)))) LeftShift (Lit (NumberLiteral 4))) Div (Lit (NumberLiteral 4.2))) BitOr (Lit (NumberLiteral 93))),ConstAssignment (Name "scientificNegative") (Lit (NumberLiteral 0.42000000000000004)),ConstAssignment (Name "infinity") (Lit (NumberLiteral Infinity)),ConstAssignment (Name "negativeInfinity") (UnaryOpPrefix MinusUop (Lit (NumberLiteral Infinity))),ConstAssignment (Name "nan") (Lit (NumberLiteral NaN))]))
-- "const num = 42\nconst str = \"literal-string\"\nconst boolTrue = true\nconst boolFalse = false\nconst arrOfNum = [1, 2, 3]\nconst arrOfStr = [\"a\", \"b\", \"c\"]\nconst arrOfBool = [true, false, true]\nconst nullLiteral = null\nconst decimal = 42\nconst decimalFloat = -42.42\nconst binary = 42\nconst octal = -42\nconst hexadecimal = 42\nconst scientific = (-42 * 100 ** 2 << 4) / 4.2 | 93\nconst scientificNegative = 0.42000000000000004\nconst infinity = Infinity\nconst negativeInfinity = -Infinity\nconst nan = NaN"

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

instance Arbitrary Var where
  arbitrary :: Gen Var
  arbitrary = QC.sized genVar

  shrink :: Var -> [Var]
  shrink (Name n) = []
  shrink (Element e1 e2) =
    [Element e1' e2 | e1' <- shrink e1] ++ [Element e1 e2' | e2' <- shrink e2]
  shrink (Dot e n) = [Dot e' n | e' <- shrink e]

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
  shrink (Var v) = Var <$> shrink v
  shrink (Lit l) = Lit <$> shrink l
  shrink (AnnotatedExpression t e) =
    e
      : [AnnotatedExpression t' e | t' <- shrink t]
      ++ [AnnotatedExpression t e' | e' <- shrink e]
  shrink (UnaryOpPrefix o e) = e : [UnaryOpPrefix o e' | e' <- shrink e]
  shrink (UnaryOpPostfix e o) = e : [UnaryOpPostfix e' o | e' <- shrink e]
  shrink (BinaryOp e1 o e2) =
    [BinaryOp e1' o e2 | e1' <- shrink e1]
      ++ [BinaryOp e1 o e2' | e2' <- shrink e2]
      ++ [e1, e2]
  shrink (FunctionExpression n es b) = undefined -- TODO: finish this
  shrink (Array es) = Array <$> shrink es

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
