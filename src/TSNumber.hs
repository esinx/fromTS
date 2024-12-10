module TSNumber where

import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

-- TODO: deal with negative 0?

data Number
  = Int Int
  | Double Double
  | Infinity
  | NInfinity
  | NaN

instance Arbitrary Number where
  arbitrary :: Gen Number
  arbitrary =
    QC.oneof
      [ Int <$> arbitrary,
        Double <$> arbitrary,
        pure Infinity,
        pure NInfinity,
        pure NaN
      ]

  shrink :: Number -> [Number]
  shrink (Int i) = Int <$> shrink i
  shrink (Double d) = Double <$> shrink d
  shrink Infinity = []
  shrink NInfinity = []
  shrink NaN = []

instance Num Number where
  (+) :: Number -> Number -> Number
  (+) NaN _ = NaN
  (+) _ NaN = NaN
  (+) (Int i) (Int j) = Int (i + j)
  (+) (Int i) (Double d) = Double (fromIntegral i + d)
  (+) (Double d) (Int i) = Double (d + fromIntegral i)
  (+) (Double d) (Double e) = Double (d + e)
  (+) Infinity NInfinity = NaN
  (+) NInfinity Infinity = NaN
  (+) Infinity _ = Infinity
  (+) _ Infinity = Infinity
  (+) NInfinity _ = NInfinity
  (+) _ NInfinity = NInfinity

  negate :: Number -> Number
  negate (Int i) = Int (negate i)
  negate (Double d) = Double (negate d)
  negate Infinity = NInfinity
  negate NInfinity = Infinity
  negate NaN = NaN

  abs :: Number -> Number
  abs (Int i) = Int (abs i)
  abs (Double d) = Double (abs d)
  abs Infinity = Infinity
  abs NInfinity = Infinity
  abs NaN = NaN

  signum :: Number -> Number
  signum (Int i) = Int (signum i)
  signum (Double d) = Int (round (signum d))
  signum Infinity = Int 1
  signum NInfinity = Int (-1)
  signum NaN = NaN

  (*) :: Number -> Number -> Number
  (*) NaN _ = NaN
  (*) _ NaN = NaN
  (*) (Int i) (Int j) = Int (i * j)
  (*) (Int i) (Double d) = Double (fromIntegral i * d)
  (*) (Double d) (Int i) = Double (d * fromIntegral i)
  (*) (Double d) (Double e) = Double (d * e)
  (*) Infinity b = case signum b of
    Int 0 -> NaN
    Int 1 -> Infinity
    Int (-1) -> NInfinity
    _ -> NaN
  (*) a Infinity = case signum a of
    Int 0 -> NaN
    Int 1 -> Infinity
    Int (-1) -> NInfinity
    _ -> NaN
  (*) NInfinity b = case signum b of
    Int 0 -> NaN
    Int 1 -> NInfinity
    Int (-1) -> Infinity
    _ -> NaN
  (*) a NInfinity = case signum a of
    Int 0 -> NaN
    Int 1 -> NInfinity
    Int (-1) -> Infinity
    _ -> NaN

  fromInteger :: Integer -> Number
  fromInteger = Int . fromInteger

instance Show Number where
  show :: Number -> String
  show (Int i) = show i
  show (Double d) = show d
  show Infinity = "Infinity"
  show NInfinity = "-Infinity"
  show NaN = "NaN"

instance Eq Number where
  (==) :: Number -> Number -> Bool
  (==) (Int i) (Int j) = i == j
  (==) (Double d) (Int i) = d == fromIntegral i
  (==) (Int i) (Double d) = fromIntegral i == d
  (==) (Double d) (Double e) = d == e
  (==) Infinity Infinity = True
  (==) NInfinity NInfinity = True
  (==) NaN NaN = False
  (==) _ _ = False

-- | Can't define total ordering (for Ord instance) because of NaN
compare :: Number -> Number -> Maybe Ordering
compare NaN _ = Nothing
compare _ NaN = Nothing
compare (Int i) (Int j) = Just $ Prelude.compare i j
compare (Int i) (Double d) = Just $ Prelude.compare (fromIntegral i) d
compare (Double d) (Int i) = Just $ Prelude.compare d (fromIntegral i)
compare (Double d) (Double e) = Just $ Prelude.compare d e
compare Infinity Infinity = Just EQ
compare Infinity _ = Just GT
compare _ Infinity = Just LT
compare NInfinity NInfinity = Just EQ
compare NInfinity _ = Just LT
compare _ NInfinity = Just GT
