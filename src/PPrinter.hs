module PPrinter where
import AST
import Data.List
import Data.Maybe
import System.Console.ANSI
import Data.Map.Strict as M hiding (splitAt, delete, foldr, map)
import List.FList

pp :: Either Error (Maybe TypedList) -> IO ()
pp (Right (Just (l, t))) = do setSGR [SetColor Foreground Vivid Green]
                              putStr (show l)
                              setSGR [SetColor Foreground Vivid Blue]
                              putStr (" <" ++ ppType t ++ ">\n")
                              setSGR [Reset]

pp (Right Nothing) = do setSGR [SetColor Foreground Vivid Green]
                        putStrLn "ok."
                        setSGR [Reset]
pp (Left err) = do setSGR [SetColor Foreground Vivid Red]
                   putStrLn (ppError err)
                   setSGR [Reset]

ppf :: Either Error (Maybe TypedList) -> IO ()
ppf (Right (Just (l, t))) = do setSGR [SetColor Foreground Vivid Green]
                               putStr (show l)
                               setSGR [SetColor Foreground Vivid Blue]
                               putStr (" <" ++ ppType t ++ ">\n")
                               setSGR [Reset]

ppf (Right Nothing) = return ()
ppf (Left err) = do setSGR [SetColor Foreground Vivid Red]
                    putStrLn (ppError err)
                    setSGR [Reset]

ppc :: [Comms] -> IO ()
ppc cmm = do setSGR [SetColor Foreground Vivid Green]
             putStrLn (foldr ((++) . show) "" cmm)
             setSGR [Reset]

ppEnv :: EnvFuncs -> EnvVars -> IO ()
ppEnv f v = do putStrLn "Function enviroment:"
               ppFuncs (toList f)
               putStrLn "Variable enviroment:"
               ppVars (toList v)


ppFuncs :: [(String, [Funcs])] -> IO ()
ppFuncs [] = putStrLn "empty"
ppFuncs [f] = do ppFuncs' f
ppFuncs (f:fns) = do ppFuncs' f
                     ppFuncs fns

ppFuncs' :: (String, [Funcs]) -> IO ()
ppFuncs' (ss, [f]) = do setSGR [SetColor Foreground Vivid Magenta]
                        putStr "def "
                        setSGR [Reset]
                        putStr $ ss ++ " = " ++ func2string f ++ ";\n"
ppFuncs' (ss, fns) = do setSGR [SetColor Foreground Vivid Magenta]
                        putStr "def "
                        setSGR [Reset]
                        putStr $ ss ++ " = " ++ unwords (map func2string fns) ++ ";\n"

func2string :: Funcs -> String
func2string (Zero L) = "zero_left"
func2string (Zero R) = "zero_right"
func2string (Delete L) = "delete_left"
func2string (Delete R) = "delete_right"
func2string (Succ L) = "succ_left"
func2string (Succ R) = "succ_right"
func2string (Rep fs) = "{" ++ unwords (map func2string fs) ++ "}"
func2string (Defined ss) = ss

ppVars :: [(String, Exp)] -> IO ()
ppVars [] = putStrLn "empty"
ppVars [exp] = do ppVars' exp
ppVars (exp:exps) = do ppVars' exp
                       ppVars exps

ppVars' :: (String, Exp) -> IO ()
ppVars' (ss, List (xs, t)) = do setSGR [SetColor Foreground Vivid Magenta]
                                putStr "let "
                                setSGR [Reset]
                                putStr $  ss ++ " = " ++ show xs
                                setSGR [SetColor Foreground Vivid Blue]
                                putStr (" <" ++ ppType t ++ ">;\n")
                                setSGR [Reset]
ppVars' (ss, Var (var, t)) = do setSGR [SetColor Foreground Vivid Magenta]
                                putStr "let "
                                setSGR [Reset]
                                putStr $ ss ++ " = " ++ var
                                setSGR [SetColor Foreground Vivid Blue]
                                putStr (" <" ++ ppType t ++ ">;\n")
                                setSGR [Reset]
ppVars' (ss, Term fs (List (xs, t))) = do setSGR [SetColor Foreground Vivid Magenta]
                                          putStr "let "
                                          setSGR [Reset]
                                          putStr $ ss ++ " = " ++ unwords (map func2string fs) ++ " : " ++ show xs
                                          setSGR [SetColor Foreground Vivid Blue]
                                          putStr (" <" ++ ppType t ++ ">;\n")
                                          setSGR [Reset]
ppVars' (ss, Term fs (Var (var, t))) = do setSGR [SetColor Foreground Vivid Magenta]
                                          putStr "let "
                                          setSGR [Reset]
                                          putStr $ ss ++ " = " ++ unwords (map func2string fs) ++ " : " ++ var
                                          setSGR [SetColor Foreground Vivid Blue]
                                          putStr (" <" ++ ppType t ++ ">;\n")
                                          setSGR [Reset]


lookupKey :: (Eq v, Eq k) => v -> M.Map k v -> [k]
lookupKey val = M.foldrWithKey cmp []
    where
      cmp key value found = if value == val then key:found
                            else found

ppType :: Type -> String
ppType DEFAULT = "Seq"
ppType t = head $ lookupKey t mapType

ppError :: Error -> String
ppError (UndefinedFunc ss) = "Error: La funcion " ++ ss ++ " no esta definida."
ppError (UndefinedVar ss) = "Error: La variable " ++ ss ++ " no esta definida."
ppError (InvalidType ss)  = "Error: El tipo de lista " ++ ss ++ " no se encuentra definido."
ppError (InvalidInfer i n) = "Error de inferencia: La lista es de longitud " ++ show i ++ " cuando tendria que ser " ++ show n ++ "."
ppError InferRep = "Error de inferencia: No se puede realizar una inferencia con la funcion '{ . }'."
ppError InferPower = "Error de inferencia: No se puede realizar una inferencia con la operacion potencia."
ppError InvalidAplication = "Error: La lista no pertenece al dominio de la funcion."
