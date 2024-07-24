module Lang where

import Data.List.NonEmpty (NonEmpty)
import Lib

-- Element of a list
type Element = Int

-- Name of variable
type Name = String

-- Orientation
data Orientation = L | R deriving (Show, Eq)

-- Sequence non-empty
type Seq a = NonEmpty a

-- Functions
data Funcs
  = Zero Orientation
  | Succ Orientation
  | Delete Orientation
  | Rep (Seq Funcs)
  | Defined Name
  | Void
  deriving (Show, Eq)

-- Expressions
data Exp func
  = Const [Element]
  | V Name
  | App (Seq func) (Exp func) Type
  | Print (Exp func)
  deriving (Show, Eq)

-- Types
data Type
  = DEFAULT
  | T String
  deriving (Show, Eq)

-- Declarations
data Decl func
  = Decl {declPos :: Pos, declName :: Name, declBody :: Exp func}
  | DeclFunc {declPos :: Pos, declName :: Name, funcBody :: Seq func}
  deriving (Show)

-- Syntactic Sugar
data SFuncs
  = SZero Orientation
  | SSucc Orientation
  | SDelete Orientation
  | SRep (Seq SFuncs)
  | SDefined Name
  | SPower Int (Seq SFuncs)
  | SVoid
  deriving (Show, Eq)

type SDecl = Decl SFuncs
