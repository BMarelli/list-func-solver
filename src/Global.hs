module Global (Env (..), initialEnviroment, defaultType) where

import Lang

-- System Environment
data Env = Env
  { lfile :: String
  -- ^ Last file loaded
  , envExp :: [(String, Exp Funcs Var)]
  -- ^ Enviroment of expressions
  , envFuncs :: [(String, Seq Funcs)]
  -- ^ Enviroment of functions
  , envTypes :: [(Type, Int)]
  -- ^ Enviroment of types
  }
  deriving (Show, Eq)

defaultType :: String
defaultType = "List"

initialEnviroment :: Env
initialEnviroment = Env "" [] [] [(DEFAULT, 0), (T defaultType, 0), (T "CList", 1), (T "Seq", 2)]
