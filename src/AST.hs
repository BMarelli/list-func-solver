module AST where
import Data.Map.Strict as M

data Comms = Def String [Funcs]
           | Const String Exp
           | Eval Exp
           | Infer Exp Int
           deriving (Show, Eq)

data Exp = List (ListElements, Type)
         | Var (String, Type)
         | Term [Funcs] Exp
         deriving (Show, Eq)

data Elements = Num Int | Generic deriving Eq
type ListElements = [Elements]

data Type = DEFAULT | INVALID String | T2 | T3 deriving (Show, Eq)

mapType :: M.Map String Type
mapType = M.fromList [("TList", DEFAULT), ("LList", T2), ("CList", T3)]

type TypedList = Maybe (ListElements, Type)

data Orientation = L | R deriving (Show, Eq)
data Funcs = Zero Orientation
           | Succ Orientation
           | Delete Orientation
           | Rep [Funcs]
           | Defined String
           deriving (Show, Eq)

instance Show Elements where
  show (Num v) = show v
  show Generic = show "X"

type EnvFuncs = M.Map String [Funcs]

type EnvVars = M.Map String Exp

data Error = UndefinedFunc String
           | UndefinedVar String
           | InvalidAplication
           | InvalidInfer Int Int
           | InferRep
           | InvalidType String
           deriving (Show, Eq)
