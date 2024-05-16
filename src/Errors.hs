module Errors (Error (..)) where

import Lib
import Text.Parsec.Error (ParseError)
import Lang

data Error
  = ErrorParse ParseError
  | ErrorPos Pos String

instance Show Error where
  show (ErrorParse s) = show s
  show (ErrorPos p s) = show p ++ " " ++ show s
