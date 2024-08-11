module Lang where

import Data.List.NonEmpty (NonEmpty)
import Lib

-- Element of a list
type Element = Int

-- Name of variable
type Name = String

-- Variable
data Var
  = Bound !Int
  | Free Name
  | Global Name
  deriving (Show, Eq)

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
data Exp func var
  = Const [Element]
  | V var
  | App (Seq func) (Exp func var) Type
  | Print (Exp func var)
  | LetIn Name (Exp func var) (Exp func var)
  deriving (Show, Eq)

-- Types
data Type
  = DEFAULT
  | T String
  deriving (Show, Eq)

-- Declarations
data Decl func var
  = Decl {declPos :: Pos, declName :: Name, declBody :: Exp func var}
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

-- Locally Nameless expression
type LNExp = Exp Funcs Var

-- Locally Nameless declaration
type LNDecl = Decl Funcs Var

-- Syntactic Sugar and fully named declaration
type SDecl = Decl SFuncs Name
