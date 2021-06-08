module PPrinter where
import AST
import Data.List
import Data.Maybe
import Data.Map.Strict as M hiding (splitAt, delete, foldr, map)

pp :: Either Error TypedList -> String
pp (Right (Just (l, t))) = show l ++ " <" ++ ppType t ++ ">"
pp (Right Nothing) = "ok."
pp (Left err) = ppError err

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
ppVars' (ss, List (xs, t)) = "const " ++ ss ++ " = " ++ show xs ++ " <" ++ ppType t ++ ">;"
ppVars' (ss, Var (var, t)) = "const " ++ ss ++ " = " ++ var ++ " <" ++ ppType t ++ ">;"
ppVars' (ss, Term fs (List (xs, t))) = "const " ++ ss ++ " = " ++ unwords (map func2string fs) ++ " : " ++ show xs ++ " <" ++ ppType t ++ ">;"
ppVars' (ss, Term fs (Var (var, t))) = "const " ++ ss ++ " = " ++ unwords (map func2string fs) ++ " : " ++ var ++ " <" ++ ppType t ++ ">;"


lookupKey :: (Eq v, Eq k) => v -> M.Map k v -> [k]
lookupKey val = M.foldrWithKey cmp []
    where
      cmp key value found = if value == val then key:found
                            else found

ppType :: Type -> String
ppType t = head $ lookupKey t mapType
-- ppType DEFAULT = let t = M.lookup "Default" mapType
--                  in ppType (fromJust t)
-- ppType t = let ss = lookupKey t mapType
--            in head ss

ppError :: Error -> String
ppError (UndefinedFunc ss) = "Error: La funcion " ++ ss ++ " no esta definida."
ppError (UndefinedVar ss) = "Error: La variable " ++ ss ++ " no esta definida."
ppError (InvalidType ss)  = "Error: El tipo de lista " ++ ss ++ " no se encuentra definido."
ppError (InvalidInfer i n) = "Error de inferencia: La lista es de longitud " ++ show i ++ " cuando tendria que ser " ++ show n ++ "."
ppError InferRep = "Error de inferencia: No se puede realizar una inferencia con la funcion '{ . }'."
ppError InvalidAplication = "Error: La lista no pertenece al dominio de la funcion."
