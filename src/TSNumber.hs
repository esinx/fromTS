module TSNumber where

import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

-- TODO: deal with negative 0?

data Number
  = Double Double
  | Infinity
  | NInfinity
  | NaN
  deriving (Show)

instance Eq Number where
  (==) :: Number -> Number -> Bool
  (==) (Double d) (Double e) = abs (d - e) < 1e-8
  (==) Infinity Infinity = True
  (==) NInfinity NInfinity = True
  (==) NaN NaN = True
  (==) _ _ = False

instance Arbitrary Number where
  arbitrary :: Gen Number
  arbitrary =
    QC.oneof
      [ Double <$> do
          max 0 <$> arbitrary, -- negative literals are generated as UnaryOpPrefix MinusUop (Lit (NumberLiteral _)
        pure Infinity,
        -- pure NInfinity,
        pure NaN
      ]

  shrink :: Number -> [Number]
  shrink (Double d) = Double <$> shrink d
  shrink Infinity = []
  shrink NInfinity = []
  shrink NaN = []

-- instance Num Number where
--   (+) :: Number -> Number -> Number
--   (+) NaN _ = NaN
--   (+) _ NaN = NaN
--   (+) (Double d) (Double e) = Double (d + e)
--   (+) Infinity NInfinity = NaN
--   (+) NInfinity Infinity = NaN
--   (+) Infinity _ = Infinity
--   (+) _ Infinity = Infinity
--   (+) NInfinity _ = NInfinity
--   (+) _ NInfinity = NInfinity

--   negate :: Number -> Number
--   negate (Double d) = Double (negate d)
--   negate Infinity = NInfinity
--   negate NInfinity = Infinity
--   negate NaN = NaN

--   abs :: Number -> Number
--   abs (Double d) = Double (abs d)
--   abs Infinity = Infinity
--   abs NInfinity = Infinity
--   abs NaN = NaN

--   signum :: Number -> Number
--   signum (Double d) = Double (signum d)
--   signum Infinity = Double 1
--   signum NInfinity = Double (-1)
--   signum NaN = NaN

--   (*) :: Number -> Number -> Number
--   (*) NaN _ = NaN
--   (*) _ NaN = NaN
--   (*) (Double d) (Double e) = Double (d * e)
--   (*) Infinity b = case signum b of
--     Double 0 -> NaN
--     Double 1 -> Infinity
--     Double (-1) -> NInfinity
--     _ -> NaN
--   (*) a Infinity = case signum a of
--     Double 0 -> NaN
--     Double 1 -> Infinity
--     Double (-1) -> NInfinity
--     _ -> NaN
--   (*) NInfinity b = case signum b of
--     Double 0 -> NaN
--     Double 1 -> NInfinity
--     Double (-1) -> Infinity
--     _ -> NaN
--   (*) a NInfinity = case signum a of
--     Double 0 -> NaN
--     Double 1 -> NInfinity
--     Double (-1) -> Infinity
--     _ -> NaN

--   fromInteger :: Integer -> Number
--   fromInteger = Double . fromInteger

-- instance Fractional Number where
--   fromRational :: Rational -> Number
--   fromRational r = Double (fromRational r)

--   recip :: Number -> Number
--   recip NaN = NaN
--   recip (Double 0) = NaN
--   recip (Double d) = Double (1 / d)
--   recip Infinity = Double 0 -- TODO: TS treats this as NaN
--   recip NInfinity = Double 0 -- TODO: TS treats this as NaN

-- instance Eq Number where
--   (==) :: Number -> Number -> Bool
--   (==) (Double d) (Double e) = d == e
--   (==) Infinity Infinity = True
--   (==) NInfinity NInfinity = True
--   (==) NaN NaN = True
--   (==) _ _ = False

-- -- | Can't define total ordering (for Ord instance) because of NaN
-- compare :: Number -> Number -> Maybe Ordering
-- compare NaN _ = Nothing
-- compare _ NaN = Nothing
-- compare (Double d) (Double e) = Just $ Prelude.compare d e
-- compare Infinity Infinity = Just EQ
-- compare Infinity _ = Just GT
-- compare _ Infinity = Just LT
-- compare NInfinity NInfinity = Just EQ
-- compare NInfinity _ = Just LT
-- compare _ NInfinity = Just GT
