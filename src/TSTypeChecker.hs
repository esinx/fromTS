module TSTypeChecker where

import TSError (error)
import TSSyntax

data TSType
  = TBoolean -- boolean
  | TNumber -- number
  | TString -- string
  | TArray TSType -- Array<T>
  | TTuple TSType TSType -- [T, U]
  | TEnum [(String, TSType)]
  | TUnknown
  | TAny
  | TVoid
  | TNull
  | TUndefined
  | TNever
  | TObject
  | TFunction [TSType] TSType

typeCheckProgram :: Block -> Either String ()
typeCheckProgram = undefined
