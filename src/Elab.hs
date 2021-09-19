module Elab where
import AST
import ListEval
import Control.Monad

desugarComms :: SComms -> Comms
desugarComms (SDef ss sfs) = Def ss (desugarFuncs sfs)
desugarComms (SLet ss sexp) = Let ss (desugarExp sexp)
desugarComms (SEval sexp) = Eval (desugarExp sexp)
desugarComms (SInfer sexp n) = Infer (desugarExp sexp) n

desugarExp :: SExp -> Exp
desugarExp (SList (l, t)) = List (l, t)
desugarExp (SVar (ss, t)) = Var (ss, t)
desugarExp (STerm sfns sexp) = Term (desugarFuncs sfns) (desugarExp sexp)

desugarFuncs :: [SFuncs] -> [Funcs]
desugarFuncs fns = join $ map desugarFuncs' fns

desugarFuncs' :: SFuncs -> [Funcs]
desugarFuncs' (SZero or) = [Zero or]
desugarFuncs' (SSucc or) = [Succ or]
desugarFuncs' (SDelete or) = [Delete or]
desugarFuncs' (SRep sfns) = [Rep (desugarFuncs sfns)]
desugarFuncs' (SDefined ss) = [Defined ss]
desugarFuncs' (SPower sfns n) = let fns = desugarFuncs sfns
                               in copyN fns n
    where
      copyN :: [Funcs] -> Int -> [Funcs]
      copyN _ 0 = []
      copyN fs n = fs ++ (copyN fs (n-1))


elab :: [SComms] -> EnvFuncs -> EnvVars -> (Either Error (Maybe TypedList), EnvFuncs, EnvVars)
elab xs f v = eval (map desugarComms xs) f v
