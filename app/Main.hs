module Main where
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import Data.Maybe
import Data.List
import Data.Char
import System.Console.Haskeline
import System.Console.ANSI
import Parse
import PPrinter ( pp, ppc, ppEnv )
import AST
import Monads
import List.FList
import ListEval

type File = Maybe FilePath

data Commands = Solve String
              | Print
              | Reload
              | LoadFile File
              | Display
              | Cmds
              | Help
              | Exit
              | None
              deriving Eq

data CommandsUse = Cmm [String] String String Commands

putLn :: String -> InputT IO ()
putLn ss = liftIO $ putStrLn ss

commands :: [CommandsUse]
commands = [
  -- Cmm [":infer"] "<exp> :: int" "infer expresion" (Infer Nothing),
  Cmm [":print"] "<exp>" "print AST of expresion" Print,
  Cmm [":reload"] "" "reload enviroment" Reload,
  Cmm [":load"] "<file>" "load a file" (LoadFile Nothing),
  Cmm [":display"] "" "display enviroment" Display,
  Cmm [":comms"] "" "display commands" Cmds,
  Cmm [":help", ":?"] "" "display the documentation" Help,
  Cmm [":quit"] "" "exit" Exit
  ]

commandsEvaluation :: String -> InputT IO Commands
commandsEvaluation cs = if isPrefixOf ":" cs 
  then do let (cmd:rest) = words cs
          let mcmd = filter (\(Cmm ss _ _ _) -> any (isPrefixOf cmd) ss) commands
          case mcmd of
            [Cmm _ _ _ f] -> case f of
                              LoadFile _ -> return $ LoadFile (Just (join rest))
                              _ -> return f
            xs -> outputStrLn "Comando ambiguo" >> return None
  else return (Solve cs)

printCommands :: String
printCommands = join $ map ((++"\n").f) commands
    where
      f :: CommandsUse -> String
      f (Cmm cs "" inf _) = let text = intercalate ", " cs
                            in text ++ replicate (30 - length text) ' ' ++ inf
      f (Cmm cs arg inf _) = let text = intercalate ", " cs ++ " " ++ arg
                             in text ++ replicate (30 - length text) ' ' ++ inf

printHelp :: String
printHelp = "TODO!"

fileManagement :: FilePath -> InputT IO (Maybe String)
fileManagement file = if isSuffixOf ".fl" file
                      then liftIO $ catch
                                (do lns <- readFile file
                                    return $ Just lns)
                                (\err -> do let err_ = show (err :: IOException)
                                            putStrLn $ "Se produjo un erro al abrir el archivo " ++ file
                                            return Nothing)
                      else outputStrLn "El archivo tiene que ser .fl" >> return Nothing

fileEvals :: FilePath -> EnvFuncs -> EnvVars -> InputT IO (EnvFuncs, EnvVars)
fileEvals file f l = do lns <- fileManagement file
                        case lns of
                          Nothing -> return (f, l)
                          Just lns -> let rest = parser lns
                                          (_, f', l') = eval rest f l
                                      in return (f', l')

printAST :: String -> InputT IO ()
printAST cs = do let (_, exp) = break isSpace cs
                 case exp of
                   [] -> outputStrLn "Tiene que ser \":p <exp>\""
                   _ -> outputStrLn $ ppc $ parser exp

repl :: EnvFuncs -> EnvVars -> InputT IO ()
repl f v = do input <- getInputLine"FL> "
              case input of
                Nothing -> return ()
                Just "" -> repl f v
                Just c -> do cmm <- commandsEvaluation c
                             case cmm of
                                Exit -> return ()
                                None -> repl f v
                                Print -> printAST c >> repl f v
                                Reload -> repl emptyEnvFuncs emptyEnvVars 
                                Solve cs -> let exp = parser cs
                                            in case eval exp f v of
                                                (Left err, f', v') -> do liftIO $ setSGR [SetColor Foreground Vivid Red]
                                                                         putLn (pp (Left err))
                                                                         liftIO $ setSGR [Reset]
                                                                         repl f' v'
                                                (Right res, f', v') -> do liftIO $ setSGR [SetColor Foreground Vivid Green]
                                                                          putLn (pp (Right res))
                                                                          liftIO $ setSGR [Reset]
                                                                          repl f' v'
                                -- Solve cs -> let exp = parser cs
                                --                 (res, f', v') = eval exp f v
                                --             in outputStrLn (pp res) >> repl f' v'
                                -- Infer cs -> let exp = parser (fromJust cs)
                                --                 (i, _, _) = infer exp f v
                                --             in outputStrLn (show i) >> repl f v
                                -- Infer cs -> let exp = parser (fromJust cs)
                                --             in case infer exp f v of
                                --                   (Left err, _, _) -> outputStrLn (fromJust cs) >> outputStrLn (pp (Left err)) >> repl f v
                                --                   _ -> let (res, f', v') = eval exp f v
                                --                        in outputStrLn (pp res) >> repl f' v'
                                -- Solve cs -> let exp = parser cs
                                --             in case infer exp f v of
                                --                 (Left err, _, _) -> outputStrLn (pp (Left err)) >> repl f v
                                --                 otherwise -> let (res, f', v') = eval exp f v
                                --                              in outputStrLn (pp res) >> repl f' v'
                                Display -> outputStrLn (ppEnv f v) >> repl f v
                                Cmds -> outputStrLn printCommands >> repl f v
                                Help -> outputStrLn printHelp >> repl f v
                                LoadFile (Just file) -> do (f', v') <- fileEvals file f v
                                                           repl f' v'
                                _ -> outputStrLn "hola" >> repl f v

main :: IO ()
main = do putStrLn "Evaluador de Funciones de Listas."
          putStrLn ":? o :help para ver la documentacion."
          putStrLn ":comms para ver los comandos."
          runInputT defaultSettings (repl emptyEnvFuncs emptyEnvVars)
