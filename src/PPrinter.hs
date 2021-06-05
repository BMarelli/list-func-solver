module PPrinter where
import AST
import Data.List
import Data.Maybe
import Data.Map.Strict as M hiding (splitAt, delete, foldr, map)

pp :: Either Error TypedList -> String
pp (Right (Just (l, t))) = show l ++ " <" ++ show t ++ ">"
pp (Right Nothing) = ""
pp (Left err) = show err

ppc :: [Comms] -> String
ppc = foldr ((++) . show) ""

ppEnv :: EnvFuncs -> EnvVars -> String
ppEnv f v = let pFunc = ppFuncs (toList f)
                pVars = ppVars (toList v)
            in "Function enviroment:\n" ++ intercalate "\n" pFunc ++ "\n" 
                ++ "Variable enviroment:\n" ++ intercalate "\n" pVars


ppFuncs :: [(String, [Funcs])] -> [String]
ppFuncs [] = ["empty"]
ppFuncs fns = map ppFuncs' fns
    where
      ppFuncs' :: (String, [Funcs]) -> String
      ppFuncs' (ss, [f]) = "def " ++ ss ++ " = " ++ func2string f ++ ";"
      ppFuncs' (ss, fns) = "def " ++ ss ++ " = " ++ unwords (map func2string fns) ++ ";"

func2string :: Funcs -> String
func2string (Zero L) = "zero_left"
func2string (Zero R) = "zero_right"
func2string (Delete L) = "delete_left"
func2string (Delete R) = "delete_right"
func2string (Succ L) = "succ_left"
func2string (Succ R) = "succ_right"
func2string (Rep fs) = "{" ++ unwords (map func2string fs) ++ "}"
func2string (Defined ss) = ss

ppVars :: [(String, Exp)] -> [String]
ppVars [] = ["empty"]
ppVars exps = map ppVars' exps

ppVars' :: (String, Exp) -> String
ppVars' (ss, List (xs, t)) = "const " ++ ss ++ " = " ++ show xs ++ " <" ++ show t ++ ">;"
ppVars' (ss, Var (var, t)) = "const " ++ ss ++ " = " ++ var ++ " <" ++ show t ++ ">;"
ppVars' (ss, Term fs (List (xs, t)) i) = "const " ++ ss ++ " = " ++ unwords (map func2string fs) ++ " : " ++ show xs ++ " <" ++ show t ++ ">;"
ppVars' (ss, Term fs (Var (var, t)) i) = "const " ++ ss ++ " = " ++ unwords (map func2string fs) ++ " : " ++ var ++ " <" ++ show t ++ ">;"
