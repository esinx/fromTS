module TSError where

data Error
  = SyntaxError String
  | TypeError String