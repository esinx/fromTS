module TSSyntax where

import Data.Map (Map)
import Data.Map qualified as Map
import TSNumber
import TSType

newtype TSTypeWrapper = TSTypeWrapper TSType
  deriving (Show)

instance Eq TSTypeWrapper where
  (==) :: TSTypeWrapper -> TSTypeWrapper -> Bool
  TSTypeWrapper t1 == TSTypeWrapper t2 = t1 =.= t2

data Literal
  = NumberLiteral Number -- 1 or 5.0 or Infinity or -Infinity or NaN
  | StringLiteral String -- "abd" or 'abd' or `abd` TODO: support templates
  | BooleanLiteral Bool -- true or false
  | NullLiteral -- null
  | UndefinedLiteral -- undefined
  | ObjectLiteral (Map String Expression) -- { x: e1, y: e2 }
  deriving (Eq, Show)

data Var
  = Name Name -- x, global variable
  | Dot Expression Name -- t.x
  | Element Expression Expression -- t[1]
  deriving (Eq, Show)

data Expression
  = Var Var -- global variables x and table indexing
  | Lit Literal -- literal values
  | UnaryOpPrefix UopPrefix Expression -- unary operators
  | UnaryOpPostfix Expression UopPostfix -- unary operators
  | BinaryOp Expression Bop Expression -- binary operators
  | FunctionCall Var [Expression] -- f(e1, ..., en)
  | FunctionInlineDeclaration [(Name, TSTypeWrapper)] (Maybe TSTypeWrapper) Block -- (x1: type1) -> { s } or function (x1: type1) { s }
  | Array [Expression] -- [e1, ..., en]
  deriving (Eq, Show)

data Statement
  = AnyExpression Expression
  | ConstAssignment Name (Maybe TSTypeWrapper) Expression -- const x: type = e
  | LetAssignment Name (Maybe TSTypeWrapper) (Maybe Expression) -- let x: type = e
  | If [(Expression, Block)] Block -- if (e) { s1 } else { s2 }
  | For (Maybe Statement) (Maybe Expression) (Maybe Expression) Block -- for (s; e1; e2) { s }
  | While Expression Block -- while (e) { s }
  | Break
  | Continue
  | Try Block (Maybe (Name, TSTypeWrapper)) Block Block -- try { s1 } catch (e) { s2 } finally { s3 }
  | FunctionDeclaration Name [(Name, TSTypeWrapper)] (Maybe TSTypeWrapper) Block -- function f(x1: type1, ..., xn: typen) { s }
  | Return (Maybe Expression)
  | TypeAlias Name TSTypeWrapper
  | InterfaceDeclaration Name TSTypeWrapper
  | Empty
  deriving (Eq, Show)

newtype Block = Block [Statement]
  deriving (Eq, Show)

instance Semigroup Block where
  (<>) :: Block -> Block -> Block
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty :: Block
  mempty = Block []

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
