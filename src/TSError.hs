module TSError where

data Error
  = SyntaxError String
  | TypeError String
  deriving (Show, Eq)
