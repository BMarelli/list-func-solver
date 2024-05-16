module Lib (Pos (..)) where

type Line = Int

type Column = Int

data Pos
  = NoPos
  | Pos !Line !Column

instance Show Pos where
  show (Pos line column) = "(" ++ show line ++ "," ++ show column ++ ")"
  show NoPos = ""
