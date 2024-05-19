module Main (main) where

import Control.Exception
import Control.Monad ()
import Control.Monad.Catch (MonadMask)
import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf)
import Elab (elab, elabExp)
import Errors
import Eval (eval)
import Global (Env (..))
import Infer (infer)
import Lang
import Lib (Pos (..))
import MonadFL
import PPrint (pp, ppDecl, ppInfer, sugar, sugarFuncs)
import Parse
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    runInputT,
  )
import System.IO (hPutStrLn, stderr)

data Command
  = Compile CompileForm
  | PPrint String
  | Infer String
  | Reload
  | Browse
  | Quit
  | Help
  | Noop

data CompileForm
  = CompileInteractive String
  | CompileFile String

data CommandUse = Cmd [String] String String (String -> Command)

prompt :: String
prompt = "FL> "

commands :: [CommandUse]
commands =
  [ Cmd [":print"] "<exp>" "pretty print the expression" PPrint,
    Cmd [":infer", ":i"] "<exp>" "infer length of expression" Infer,
    Cmd [":reload"] "" "reload enviroment" (const Reload),
    Cmd [":load"] "<file>" "load a file" (Compile . CompileFile),
    Cmd [":browse"] "" "display enviroment" (const Browse),
    Cmd [":help", ":?"] "" "display commands and the documentation" (const Help),
    Cmd [":quit"] "" "exit" (const Quit)
  ]

interpretCommand :: String -> IO Command
interpretCommand x =
  if ":" `isPrefixOf` x
    then do
      let (cmd, t') = break isSpace x
          t = dropWhile isSpace t'
      --  find matching commands
      let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
      case matching of
        [] -> do
          putStrLn ("Unknown command `" ++ cmd ++ "'. Use :? for help.")
          return Noop
        [Cmd _ _ _ f] ->
          do return (f t)
        _ -> do
          putStrLn
            ( "Ambiguous command, could be "
                ++ intercalate ", " ([head cs | Cmd cs _ _ _ <- matching])
                ++ "."
            )
          return Noop
    else return (Compile (CompileInteractive x))

help :: [CommandUse] -> String
help cs =
  "Commands list:  Every command can be abbreviated to :c where\n"
    ++ "c is the first character of the command.\n\n"
    ++ "<expr>                  eval an expression\n"
    ++ "let <var> = <expr>      define a variable\n"
    ++ "def <var> = <seq func>  define a function\n"
    ++ unlines
      ( map
          ( \(Cmd c a d _) ->
              let ct = intercalate ", " (map (++ if null a then "" else " " ++ a) c)
               in ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
          )
          cs
      )

handleSDecl :: MonadFL m => SDecl -> m (Decl Funcs)
handleSDecl d = do
  let d' = elab d
  case d' of
    Decl _ name body -> addExp name body >> return d'
    DeclFunc _ name body -> addFunc name body >> return d'

handleExpr :: MonadFL m => Exp SFuncs -> m ()
handleExpr e = do
  let e' = elabExp e
  r <- eval e'
  printFL (pp (Const r))

parseIO :: MonadFL m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
  Left e -> throwError (ErrorParse e)
  Right r -> return r

loadFile :: MonadFL m => FilePath -> m [SDecl]
loadFile f = do
  let filename = reverse (dropWhile isSpace (reverse f))
  x <-
    liftIO $
      catch
        (readFile filename)
        ( \e -> do
            let err = show (e :: IOException)
            hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
            return ""
        )
  setLastFile filename
  parseIO filename program x

compileFile :: MonadFL m => FilePath -> m ()
compileFile = loadFile >=> mapM_ handleSDecl

compileExpr :: MonadFL m => String -> m ()
compileExpr x = do
  p <- parseIO "<interactive>" declOrExpr x
  case p of
    Left d -> void (handleSDecl d)
    Right e -> void (handleExpr e)

inferExpr :: MonadFL m => String -> m ()
inferExpr x = do
  se <- parseIO "<interactive>" expr x
  let e = elabExp se
  t <- infer 0 e
  printFL (ppInfer t)

prittyPrint :: MonadFL m => String -> m ()
prittyPrint x = do
  se <- parseIO "<interactive>" expr x
  case se of
    Const r -> printFL (pp (Const r))
    V n -> maybe (failFL ("Variable " ++ n ++ " not found.")) (printFL . pp . sugar) =<< lookUpExp n
    App fs e t -> printFL (pp (App fs e t))

handleCommand :: MonadFL m => Command -> m Bool
handleCommand cmd = do
  s <- get
  case cmd of
    Quit -> return False
    Noop -> return True
    Help -> do
      printFL (help commands)
      return True
    Browse -> do
      printFL "Environment:"
      mapM_ (\(n, e) -> printFL (ppDecl (Decl NoPos n (sugar e)))) (envExp s)
      mapM_ (\(n, fs) -> printFL (ppDecl (DeclFunc NoPos n (sugarFuncs fs)))) (envFuncs s)
      return True
    Reload -> compileFile (lfile s) >> return True
    PPrint e -> prittyPrint e >> return True
    Infer e -> inferExpr e >> return True
    Compile c -> do
      case c of
        CompileInteractive e -> compileExpr e
        CompileFile f -> put (s {lfile = f}) >> compileFile f
      return True

repl :: (MonadFL m, MonadMask m) => InputT m ()
repl = do
  i <- getInputLine prompt
  case i of
    Nothing -> return ()
    Just "" -> repl
    Just x -> do
      cmd <- liftIO $ interpretCommand x
      cont <- lift $ catchErrors $ handleCommand cmd
      maybe repl (`when` repl) cont

main :: IO ()
main = do
  putStrLn "List Function Evaluator."
  putStrLn ":? o :help to know the commands and how to use the program."
  void (runFL (compileFile "test/testcases/Prelude.fl" >> runInputT defaultSettings repl))
