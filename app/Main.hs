module Main where
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import Data.Maybe
import Data.List
import Data.Char
import Data.Map.Strict as M hiding (filter, map, drop)
import System.Console.Haskeline
import System.Console.ANSI
import Parse
import PPrinter ( pp, ppc, ppEnv )
import AST
import Monads
import List.FList
import Elab

type File = Maybe FilePath

data Commands = Solve String
              | Clear String
              | Print
              | Reload
              | LoadFile File
              | Display
              | Help
              | Exit
              | None
              deriving Eq

data CommandsUse = Cmm [String] String String Commands

putLn :: String -> InputT IO ()
putLn ss = liftIO $ putStrLn ss

commands :: [CommandsUse]
commands = [
  Cmm [":print"] "<exp>" "print AST of expresion" Print,
  Cmm [":clear"] "<exp>" "clear variable or funtion from enviroment" (Clear ""),
  Cmm [":reload"] "" "reload enviroment" Reload,
  Cmm [":load"] "<file>" "load a file" (LoadFile Nothing),
  Cmm [":display"] "" "display enviroment" Display,
  Cmm [":help", ":?"] "" "display commands and the documentation" Help,
  Cmm [":quit"] "" "exit" Exit
  ]

commandsEvaluation :: String -> InputT IO Commands
commandsEvaluation cs = if isPrefixOf ":" cs 
  then do let (cmd:rest) = words cs
          let mcmd = filter (\(Cmm ss _ _ _) -> any (isPrefixOf cmd) ss) commands
          case mcmd of
            [Cmm _ _ _ f] -> case f of
                              LoadFile _ -> return $ LoadFile (Just (join rest))
                              Clear _ -> return $ Clear (join rest)
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
printHelp = printCommands ++"\nTODO!"

fileManagement :: FilePath -> InputT IO (Maybe String)
fileManagement file = if isSuffixOf ".fl" file
                      then liftIO $ catch
                                (do lns <- readFile file
                                    return $ Just lns)
                                (\err -> do let err_ = show (err :: IOException)
                                            putStrLn $ "Se produjo un error al abrir el archivo " ++ file
                                            return Nothing)
                      else do liftIO $ setSGR [SetColor Foreground Vivid Red]
                              putLn "El archivo tiene que ser .fl"
                              liftIO $ setSGR [Reset]
                              return Nothing

fileEvals :: FilePath -> EnvFuncs -> EnvVars -> InputT IO (EnvFuncs, EnvVars)
fileEvals file f l = do lns <- fileManagement file
                        case lns of
                          Nothing -> return (f, l)
                          Just lns -> do liftIO $ setSGR [SetColor Foreground Vivid Green]
                                         putLn $ "Se abrio el archivo "++ file ++ " correctamente!."
                                         liftIO $ setSGR [Reset]
                                         let rest = parser lns
                                             (_, f', l') = elab rest f l
                                         return (f', l')

-- Cambiar esto (comando propio)
printAST :: EnvFuncs -> EnvVars -> String -> InputT IO ()
printAST f v cs = do let (_, exp) = break isSpace cs
                     case exp of
                       [] -> outputStrLn "Tiene que ser \":p <exp>\""
                       _ -> do let exp' = parser (drop 3 cs)
                               case elab exp' f v of
                                 (Left err, f', v') -> do liftIO $ pp (Left err)
                                 (Right res, f', v') -> do liftIO $ ppc (map desugarComms exp')

clearFromEnviroment :: String -> EnvFuncs -> EnvVars -> InputT IO (EnvFuncs, EnvVars)
clearFromEnviroment ss f v = if M.member ss f then return (M.delete ss f, v)
                             else return (f, M.delete ss v)

repl :: EnvFuncs -> EnvVars -> InputT IO ()
repl f v = do input <- getInputLine"FL> "
              case input of
                Nothing -> return ()
                Just "" -> repl f v
                Just c -> do cmm <- commandsEvaluation c
                             case cmm of
                              Exit -> return ()
                              None -> repl f v
                              Print -> printAST f v c >> repl f v
                              Clear cs -> do (f', v') <- clearFromEnviroment cs f v
                                             repl f' v'
                              Reload -> repl emptyEnvFuncs emptyEnvVars 
                              Solve cs -> let exp = parser cs
                                          in case elab exp f v of
                                              (Left err, f', v') -> do liftIO (pp (Left err))
                                                                       repl f' v'
                                              (Right res, f', v') -> do liftIO (pp (Right res))
                                                                        repl f' v'
                              Display -> liftIO (ppEnv f v) >> repl f v
                              Help -> outputStrLn printHelp >> repl f v
                              LoadFile (Just file) -> do (f', v') <- fileEvals file f v
                                                         repl f' v'
                              _ -> outputStrLn "hola" >> repl f v

main :: IO ()
main = do putStrLn "Evaluador de Funciones de Listas."
          putStrLn ":? o :help para conocer los comandos y como utilizar el programa."
          (f, v) <- runInputT defaultSettings $ fileEvals "Ejemplos/Prelude.fl" emptyEnvFuncs emptyEnvVars
          runInputT defaultSettings (repl f v)
