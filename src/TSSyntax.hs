module TSSyntax where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map, fromList)
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
  | Empty
  deriving (Eq, Show)

data Expression
  = Var Var -- global variables x and table indexing
  | Lit Literal -- literal values
  | AnnotatedExpression TSType Expression -- e : type
  | UnaryOpPrefix UopPrefix Expression -- unary operators
  | UnaryOpPostfix Expression UopPostfix -- unary operators
  | BinaryOp Expression Bop Expression -- binary operators
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
          ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
      ppPrec _ e' = pp e'
      ppParens b = if b then PP.parens else id
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
  pp Empty = PP.empty

level :: Bop -> Int
level b = case b of
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
