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

newtype TSTypeWrapper = TSTypeWrapper TSType
  deriving (Show)

instance Eq TSTypeWrapper where
  (==) :: TSTypeWrapper -> TSTypeWrapper -> Bool
  TSTypeWrapper t1 == TSTypeWrapper t2 = t1 =.= t2

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
  | TypeAlias Name TSTypeWrapper
  | InterfaceDeclaration Name TSTypeWrapper
  | Empty
  deriving (Eq, Show)

data Expression
  = Var Var -- global variables x and table indexing
  | Lit Literal -- literal values
  | AnnotatedExpression TSTypeWrapper Expression -- e : type
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
  pp :: Bool -> a -> Doc

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
render :: (PP a) => Bool -> a -> String
render b = PP.render . pp b

pretty :: (PP a) => a -> String
pretty = render True

transpileTS :: (PP a) => a -> String
transpileTS = render False

-- | Compact version. Displays its argument without newlines.
oneLine :: (PP a) => Bool -> a -> String
oneLine b = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp b

commaSpace :: Doc
commaSpace = PP.text ", "

instance PP Bool where
  pp :: Bool -> Bool -> Doc
  pp _ True = PP.text "true"
  pp _ False = PP.text "false"

instance PP String where
  pp :: Bool -> String -> Doc
  pp _ = PP.text

instance PP Int where
  pp :: Bool -> Int -> Doc
  pp _ = PP.int

instance PP Double where
  pp :: Bool -> Double -> Doc
  pp _ d =
    if d == fromInteger (round d)
      then PP.int (round d)
      else PP.double d

instance PP Number where
  pp :: Bool -> Number -> Doc
  pp _ (Double d) =
    if d == fromInteger (round d)
      then PP.text $ show (round d)
      else PP.text $ show d
  pp _ Infinity = PP.text "Infinity"
  pp _ NInfinity = PP.text "-Infinity"
  pp _ NaN = PP.text "NaN"

instance PP Var where
  pp :: Bool -> Var -> Doc
  pp _ (Name n) = PP.text n
  pp b (Dot (Var v) k) = pp b v <> PP.text "." <> pp b k
  pp b (Dot t k) = PP.parens (pp b t) <> PP.text "." <> pp b k
  pp b (Element (Var v) k) = pp b v <> PP.brackets (pp b k)
  pp b (Element t k) = PP.parens (pp b t) <> PP.brackets (pp b k)

instance PP Literal where
  pp :: Bool -> Literal -> Doc
  pp b (NumberLiteral i) = pp b i
  pp _ (StringLiteral s) = PP.text ("\"" <> s <> "\"")
  pp t (BooleanLiteral b) = pp t b
  pp _ NullLiteral = PP.text "null"
  pp _ UndefinedLiteral = PP.text "undefined"
  pp b (ObjectLiteral m) = PP.braces (PP.space <> PP.sep (PP.punctuate PP.comma (map ppa (Map.toList m))) <> PP.space)
    where
      ppa (s, v) = PP.text ("\"" ++ s ++ "\"") <> (PP.colon <+> pp b v)

instance PP TSTypeWrapper where
  pp :: Bool -> TSTypeWrapper -> Doc
  pp b (TSTypeWrapper t) = pp b t

instance PP TSType where
  pp :: Bool -> TSType -> Doc
  pp b ty = ppPrec 0 b ty
    where
      ppPrec :: Int -> Bool -> TSType -> Doc
      ppPrec _ _ TBoolean = PP.text "boolean"
      ppPrec _ _ (TBooleanLiteral b) = if b then PP.text "true" else PP.text "false"
      ppPrec _ _ TNumber = PP.text "number"
      ppPrec _ b (TNumberLiteral n) = pp b n
      ppPrec _ _ TString = PP.text "string"
      ppPrec _ b (TStringLiteral s) = pp b $ "\"" ++ s ++ "\""
      ppPrec _ b (TTuple ts) = PP.brackets (PP.sep (PP.punctuate PP.comma (map (pp b) ts)))
      ppPrec _ _ TBracket = PP.text "{}"
      ppPrec _ _ TObject = PP.text "object"
      ppPrec _ b (TTypeAlias n) = pp b n
      ppPrec _ b (TUserObject m) =
        PP.vcat
          [ PP.char '{',
            PP.nest 2 $ PP.sep (PP.punctuate PP.comma (map ppa (Map.toList m))) <> PP.space,
            PP.char '}'
          ]
        where
          ppa (s, v) = PP.text s <> (PP.colon <+> pp b v)
      ppPrec _ _ TUnknown = PP.text "unknown"
      ppPrec _ _ TAny = PP.text "any"
      ppPrec _ _ TNever = PP.text "never"
      ppPrec _ _ TVoid = PP.text "void"
      ppPrec _ _ TNull = PP.text "null"
      ppPrec _ _ TUndefined = PP.text "undefined"
      ppPrec n b (TArray t) =
        let prec = precedence (TArray t)
            childDoc = ppPrec prec b t
            doc = childDoc <> PP.text "[]"
         in ppParens (prec < n) doc
      ppPrec n b (TUnion ts) =
        let prec = precedence (TUnion ts)
            docs = map (ppPrec (prec + 1) b) ts
            doc = PP.sep (PP.punctuate (PP.text " |") docs)
         in ppParens (prec < n) doc
      ppPrec n b (TIntersection ts) =
        let prec = precedence (TIntersection ts)
            docs = map (ppPrec (prec + 1) b) ts
            doc = PP.sep (PP.punctuate (PP.text " &") docs)
         in ppParens (prec < n) doc
      ppParens b = if b then PP.parens else id

isBase :: Expression -> Bool
isBase Var {} = True
isBase Lit {} = True
isBase _ = False

hasSpace :: UopPrefix -> Bool
hasSpace TypeOf = True
hasSpace Void = True
hasSpace _ = False

instance PP UopPrefix where
  pp :: Bool -> UopPrefix -> Doc
  pp _ Not = PP.text "!"
  pp _ BitNeg = PP.text "~"
  pp _ TypeOf = PP.text "typeof"
  pp _ Spread = PP.text "..."
  pp _ DecPre = PP.text "--"
  pp _ IncPre = PP.text "++"
  pp _ PlusUop = PP.char '+'
  pp _ MinusUop = PP.char '-'
  pp _ Void = PP.text "void"

instance PP UopPostfix where
  pp :: Bool -> UopPostfix -> Doc
  pp _ DecPost = PP.text "--"
  pp _ IncPost = PP.text "++"

instance PP Bop where
  pp :: Bool -> Bop -> Doc
  pp _ Assign = PP.equals
  pp _ PlusBop = PP.char '+'
  pp _ PlusAssign = PP.text "+="
  pp _ MinusBop = PP.char '-'
  pp _ MinusAssign = PP.text "-="
  pp _ Times = PP.char '*'
  pp _ TimesAssign = PP.text "*="
  pp _ Div = PP.char '/'
  pp _ DivAssign = PP.text "/="
  pp _ Mod = PP.char '%'
  pp _ ModAssign = PP.text "%="
  pp _ Exp = PP.text "**"
  pp _ ExpAssign = PP.text "**="
  pp _ BitAnd = PP.char '&'
  pp _ BitAndAssign = PP.text "&="
  pp _ BitOr = PP.char '|'
  pp _ BitOrAssign = PP.text "|="
  pp _ BitXor = PP.char '^'
  pp _ BitXorAssign = PP.text "^="
  pp _ LeftShift = PP.text "<<"
  pp _ LeftShiftAssign = PP.text "<<="
  pp _ RightShift = PP.text ">>"
  pp _ RightShiftAssign = PP.text ">>="
  pp _ UnsignedRightShift = PP.text ">>>"
  pp _ UnsignedRightShiftAssign = PP.text ">>>="
  pp _ And = PP.text "&&"
  pp _ AndAssign = PP.text "&&="
  pp _ Or = PP.text "||"
  pp _ OrAssign = PP.text "||="
  pp _ NullishCoalescing = PP.text "??"
  pp _ NullishCoalescingAssign = PP.text "??="
  pp _ Eq = PP.text "=="
  pp _ Neq = PP.text "!="
  pp _ EqStrict = PP.text "==="
  pp _ NeqStrict = PP.text "!=="
  pp _ Gt = PP.char '>'
  pp _ Ge = PP.text ">="
  pp _ Lt = PP.char '<'
  pp _ Le = PP.text "<="
  pp _ In = PP.text "in"
  pp _ InstanceOf = PP.text "instanceof"

instance PP Expression where
  pp :: Bool -> Expression -> Doc
  pp b (Var v) = pp b v
  pp b (Lit l) = pp b l
  pp False (AnnotatedExpression _ e) = pp False e
  pp b (AnnotatedExpression t e) = pp b e <> (PP.colon <+> pp b t)
  pp b (UnaryOpPrefix uop e) =
    if hasSpace uop
      then pp b uop <+> if isBase e then pp b e else PP.parens (pp b e)
      else pp b uop <> if isBase e then pp b e else PP.parens (pp b e)
  pp b (UnaryOpPostfix e uop) = (if isBase e then pp b e else PP.parens (pp b e)) <> pp b uop
  pp b e@(BinaryOp {}) = ppPrec 0 e
    where
      ppPrec n (BinaryOp e1 bop e2) =
        ppParens (level bop < n) $
          ppPrec (level bop) e1 <+> pp b bop <+> ppPrec (level bop + 1) e2
      ppPrec _ e' = pp b e'
      ppParens b = if b then PP.parens else id
  pp b (Array es) = PP.brackets (PP.hsep (PP.punctuate PP.comma (map (pp b) es)))

ppSS :: Bool -> [Statement] -> Doc
ppSS b ss = PP.vcat (map (pp b) ss)

instance PP Block where
  pp :: Bool -> Block -> Doc
  pp b (Block [s]) = pp b s
  pp b (Block ss) = ppSS b ss

isEmptyBlock :: Bool -> Block -> Bool
isEmptyBlock t b = pp t b == PP.empty

instance PP Statement where
  pp :: Bool -> Statement -> Doc
  pp b (AnyExpression e) = pp b e <> PP.semi
  pp False (ConstAssignment v (AnnotatedExpression _ e)) = (PP.text "const" <+> pp False v <+> PP.equals <+> pp False e) <> PP.semi
  pp b (ConstAssignment v (AnnotatedExpression name e)) = (PP.text "const" <+> (pp b v <> (PP.colon <+> pp b name <+> PP.equals <+> pp b e))) <> PP.semi
  pp b (ConstAssignment v e) = (PP.text "const" <+> pp b v <+> PP.equals <+> pp b e) <> PP.semi
  pp False (LetAssignment v e) = (PP.text "let" <+> pp False v <+> PP.equals <+> pp False e) <> PP.semi
  pp b (LetAssignment v (AnnotatedExpression name e)) = (PP.text "let" <+> (pp b v <> (PP.colon <+> pp b name <+> PP.equals <+> pp b e))) <> PP.semi
  pp b (LetAssignment v e) = (PP.text "let" <+> pp b v <+> PP.equals <+> pp b e) <> PP.semi
  pp b (If [] elseBlock) =
    PP.vcat
      [ PP.text "if (false) {",
        PP.text "} else {",
        PP.nest 2 (pp b elseBlock),
        PP.char '}'
      ]
  pp t (If ((c, b) : rest) elseBlock) =
    PP.vcat $
      [ PP.text "if" <+> PP.parens (pp t c) <+> PP.char '{',
        PP.nest 2 (pp t b)
      ]
        ++ concatMap
          ( \(cond, blk) ->
              [ PP.text "} else if" <+> PP.parens (pp t cond) <+> PP.char '{',
                PP.nest 2 (pp t blk)
              ]
          )
          rest
        ++ ( if not (isEmptyBlock t elseBlock)
               then [PP.text "} else {", PP.nest 2 (pp t elseBlock)]
               else []
           )
        ++ [PP.char '}']
  pp t (For init guard update blk) =
    PP.vcat
      [ PP.text "for" <+> PP.parens (pp t init <+> (pp t guard <> (PP.semi <+> pp t update))) <+> PP.char '{',
        PP.nest 2 (pp t blk),
        PP.char '}'
      ]
  pp b (While guard blk) =
    PP.vcat
      [ PP.text "while" <+> PP.parens (pp b guard) <+> PP.char '{',
        PP.nest 2 (pp b blk),
        PP.char '}'
      ]
  pp _ Break = PP.text "break" <> PP.semi
  pp _ Continue = PP.text "continue" <> PP.semi
  pp b (Try tryBlock mbE catchBlock finallyBlock) =
    PP.vcat $
      [ PP.text "try {",
        PP.nest 2 (pp b tryBlock)
      ]
        ++ ( case mbE of
               Nothing ->
                 if not (isEmptyBlock b catchBlock) || isEmptyBlock b finallyBlock
                   then [PP.text "} catch {", PP.nest 2 (pp b catchBlock)]
                   else []
               Just e ->
                 [ PP.text "} catch" <+> PP.parens (pp b e) <+> PP.char '{',
                   PP.nest 2 (pp b catchBlock)
                 ]
           )
        ++ ( if not (isEmptyBlock b finallyBlock)
               then [PP.text "} finally {", PP.nest 2 (pp b finallyBlock)]
               else []
           )
        ++ [PP.char '}']
  pp _ (Return Nothing) = PP.text "return" <> PP.semi
  pp b (Return (Just e)) = (PP.text "return" <+> pp b e) <> PP.semi
  pp False (TypeAlias _ _) = PP.empty
  pp b (TypeAlias n t) = (PP.text "type" <+> pp b n <+> PP.equals <+> pp b t) <> PP.semi
  pp False (InterfaceDeclaration _ _) = PP.empty
  pp b (InterfaceDeclaration n t) = (PP.text "interface" <+> pp b n <+> pp b t) <> PP.semi
  pp _ Empty = PP.empty

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

precedence :: TSType -> Int
precedence TUnion {} = 1
precedence TIntersection {} = 2
precedence TArray {} = 3
precedence _ = 4
