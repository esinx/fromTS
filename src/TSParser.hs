module TSParser where

import Control.Applicative
import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import TSSyntax
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.Parsec qualified as P
import Text.Parsec.Expr qualified as P.Expr
import Text.Parsec.String (Parser)
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

wsP :: Parser a -> Parser a
wsP p = p <* many P.space

-- test_wsP :: Test
-- test_wsP =
--   TestList
--     [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
--       P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
--     ]

-- >>> runTestTT test_wsP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

prop_roundtrip_val :: Literal -> Bool
prop_roundtrip_val v = undefined

-- P.parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = undefined

-- P.parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = undefined

-- P.parse statementP (pretty s) == Right s
