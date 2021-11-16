module Elab where

import AST
import ListEval
import GHC.Base (join)

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
desugarFuncs [] = []
desugarFuncs ((SZero op) : fns) = Zero op : desugarFuncs fns
desugarFuncs ((SSucc op) : fns) = Succ op : desugarFuncs fns
desugarFuncs ((SDelete op) : fns) = Delete op : desugarFuncs fns
desugarFuncs ((SRep sfns) : fns) = Rep (desugarFuncs sfns) : desugarFuncs fns
desugarFuncs ((SDefined ss) : fns) = Defined ss : desugarFuncs fns
desugarFuncs ((SPower sfns n) : fns) = (join . replicate n) (desugarFuncs sfns) ++ desugarFuncs fns

elab :: [SComms] -> EnvFuncs -> EnvVars -> (Either Error (Maybe TypedList), EnvFuncs, EnvVars)
elab xs f v = eval (map desugarComms xs) f v
