module Global (Env (..), initialEnviroment, defaultType) where

import Lang

-- System Environment
data Env = Env
  { -- | Last file loaded
    lfile :: String,
    -- | Enviroment of expressions
    envExp :: [(String, Exp Funcs)],
    -- | Enviroment of functions
    envFuncs :: [(String, Seq Funcs)],
    -- | Enviroment of types
    envTypes :: [(Type, Int)]
  }
  deriving (Show, Eq)

defaultType :: String
defaultType = "List"

initialEnviroment :: Env
initialEnviroment = Env "" [] [] [(DEFAULT, 0), (T defaultType, 0), (T "CList", 1), (T "Seq", 2)]
