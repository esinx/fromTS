module TSGen where

import Control.Monad (guard)
import Control.Monad.Reader
import Data.Char qualified as Char
import Data.Map qualified as Map
import TSSyntax
import TSType
import TSTypeChecker (typeCheckExpr)
import Test.QuickCheck
import Test.QuickCheck qualified as QC

-- | Generate a small set of names for generated tests. These names are guaranteed to not include
-- reserved words
genName :: Gen Name
genName = QC.elements ["x", "X", "y", "x0", "X0", "xy", "XY"]

-- | Generate a string literal, being careful about the characters that it may contain
genStringLit :: Gen String
genStringLit = QC.elements ["", "a", "b", "c", "ab", "cd", "d", "katrina", "eunsoo", "jordan"]

genLiteral :: Int -> Gen Literal
genLiteral 0 =
  QC.oneof
    [ NumberLiteral <$> arbitrary,
      BooleanLiteral <$> arbitrary,
      pure NullLiteral,
      pure UndefinedLiteral,
      StringLiteral <$> genStringLit
    ]
genLiteral n =
  QC.frequency
    [ (n, NumberLiteral <$> arbitrary),
      (n, BooleanLiteral <$> arbitrary),
      (1, pure NullLiteral),
      (1, pure UndefinedLiteral),
      (n, StringLiteral <$> genStringLit),
      (n, ObjectLiteral . Map.fromList <$> QC.vectorOf 1 ((,) <$> genStringLit <*> genExp n'))
    ]
  where
    n' = n `div` 2

-- | Generate a size-controlled global variable or table field
genVar :: Int -> Gen Var
genVar 0 = Name <$> genName
genVar n =
  QC.frequency
    [ (1, Name <$> genName),
      (n, Dot <$> genExp n' <*> genName),
      (n, Element <$> genExp n' <*> genExp n')
    ]
  where
    n' = n `div` 2

makeGenMaybe :: Gen a -> Gen (Maybe a)
makeGenMaybe g = QC.oneof [return Nothing, Just <$> g]

-- | Generate a size-controlled expression
genExp :: Int -> Gen Expression
genExp 0 = QC.oneof [Var <$> genVar 0, Lit <$> arbitrary]
genExp n =
  QC.frequency
    [ (1, Var <$> genVar n),
      (1, Lit <$> arbitrary),
      --   (n, AnnotatedExpression <$> arbitrary <*> genExp n'),
      (n, UnaryOpPrefix <$> arbitrary <*> genExp n'),
      (n, UnaryOpPostfix <$> genExp n' <*> arbitrary),
      (n, BinaryOp <$> genExp n' <*> arbitrary <*> genExp n'),
      (n, Array <$> QC.vectorOf 1 (genExp n'))
    ]
  where
    n' = n `div` 2

genAnnotatedExp :: Int -> Gen Expression
genAnnotatedExp n = QC.oneof [AnnotatedExpression <$> arbitrary <*> genExp n, genExp n]

genCatch :: Gen Expression
genCatch = QC.oneof [Var . Name <$> genName, AnnotatedExpression <$> arbitrary <*> (Var . Name <$> genName)]

-- | Generate a size-controlled statement
genStatement :: Int -> Gen Statement
genStatement n | n <= 1 = QC.oneof [ConstAssignment <$> genVar 0 <*> genExp 0]
genStatement n =
  QC.frequency
    [ (1, (ConstAssignment . Name <$> genName) <*> genAnnotatedExp n'),
      (1, (LetAssignment . Name <$> genName) <*> genAnnotatedExp n'),
      (1, TypeAlias <$> genName <*> arbitrary),
      (1, InterfaceDeclaration <$> genName <*> arbitrary),
      (n, AnyExpression <$> genExp n'),
      (n, If <$> QC.vectorOf 1 ((,) <$> genExp n' <*> genBlock n') <*> genBlock n'),
      (n, Return <$> makeGenMaybe (genExp n')),
      -- generate loops half as frequently as if statements
      (n', While <$> genExp n' <*> genBlock n'),
      ( n',
        Try
          <$> genBlock n'
          <*> makeGenMaybe genCatch
          <*> genBlock n'
          <*> genBlock n'
      ),
      (n', For <$> genStatement n' <*> genExp n' <*> genExp n' <*> genBlock n')
    ]
  where
    n' = n `div` 2

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
  shrink (AnyExpression e) = [AnyExpression e' | e' <- shrink e]
  shrink (ConstAssignment v e) = [ConstAssignment v' e | v' <- shrink v] ++ [ConstAssignment v e' | e' <- shrink e]
  shrink (LetAssignment v e) = [LetAssignment v' e | v' <- shrink v] ++ [LetAssignment v e' | e' <- shrink e]
  shrink (If ebs elseBlock) =
    [If ebs' elseBlock | ebs' <- shrink ebs] ++ [If ebs elseBlock' | elseBlock' <- shrink elseBlock]
  shrink (For v e guard b) = [For v' e guard b | v' <- shrink v] ++ [For v e' guard b | e' <- shrink e] ++ [For v e guard b' | b' <- shrink b]
  shrink (While e b) = [While e' b | e' <- shrink e] ++ [While e b' | b' <- shrink b]
  shrink (Try b maybeE catchBlock finallyBlock) =
    [Try b' maybeE catchBlock finallyBlock | b' <- shrink b]
      ++ [Try b maybeE' catchBlock finallyBlock | maybeE' <- shrink maybeE]
      ++ [Try b maybeE catchBlock' finallyBlock | catchBlock' <- shrink catchBlock]
      ++ [Try b maybeE catchBlock finallyBlock' | finallyBlock' <- shrink finallyBlock]
  shrink (TypeAlias n t) = [TypeAlias n t' | t' <- shrink t]
  shrink (InterfaceDeclaration n t) = [InterfaceDeclaration n t' | t' <- shrink t]
  shrink (Return e) = [Return e' | e' <- shrink e]
  shrink Empty = []
  shrink Break = []
  shrink Continue = []

-- | access the first statement in a block, if one exists
first :: Block -> [Statement]
first (Block []) = []
first (Block (x : _)) = [x]

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

shrinkMapExpr :: Map.Map String Expression -> [Map.Map String Expression]
shrinkMapExpr m = [Map.fromList [(s, e') | e' <- shrink e] | (s, e) <- Map.toList m]

instance Arbitrary Literal where
  arbitrary :: Gen Literal
  arbitrary = QC.sized genLiteral

  shrink :: Literal -> [Literal]
  shrink (NumberLiteral n) = NumberLiteral <$> shrink n
  shrink (BooleanLiteral b) = BooleanLiteral <$> shrink b
  shrink NullLiteral = []
  shrink UndefinedLiteral = []
  shrink (StringLiteral s) = StringLiteral <$> shrinkStringLit s
  shrink (ObjectLiteral m) = ObjectLiteral <$> shrinkMapExpr m

sampleVar :: IO ()
sampleVar = QC.sample' (arbitrary :: Gen Var) >>= mapM_ (print . pp True)

sampleExp :: IO ()
sampleExp = QC.sample' (arbitrary :: Gen Expression) >>= mapM_ (print . pp True)

sampleStat :: IO ()
sampleStat = QC.sample' (arbitrary :: Gen Statement) >>= mapM_ (print . pp True)

quickCheckN :: (QC.Testable prop) => Int -> prop -> IO ()
quickCheckN n = QC.quickCheckWith $ QC.stdArgs {QC.maxSuccess = n, QC.maxSize = 100}
