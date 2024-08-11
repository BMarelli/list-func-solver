module Elab (elab, elabExp, transformFuncs, transformPower) where

import Data.List.NonEmpty
import GHC.Base (join)
import Lang
import Subst (close)
import Prelude hiding (filter, map)

transformPower :: Seq SFuncs -> Seq SFuncs
transformPower = (go =<<)
 where
  go :: SFuncs -> Seq SFuncs
  go (SPower 0 _) = singleton SVoid
  go (SPower n fs) = join . fromList . replicate n . transformPower $ fs
  go fs = pure fs

transformFunc :: SFuncs -> Funcs
transformFunc (SZero o) = Zero o
transformFunc (SSucc o) = Succ o
transformFunc (SDelete o) = Delete o
transformFunc (SRep sfns) = Rep (transformFuncs sfns)
transformFunc (SDefined name) = Defined name
transformFunc SVoid = Void
transformFunc _ = error "transformFunc: not implemented"

transformFuncs :: Seq SFuncs -> Seq Funcs
transformFuncs = map transformFunc . transformPower

elabExp' :: Exp SFuncs Name -> Exp Funcs Name
elabExp' (Const xs) = Const xs
elabExp' (V name) = V name
elabExp' (App sfns e ty) = App (transformFuncs sfns) (elabExp' e) ty
elabExp' (Print e) = Print (elabExp' e)
elabExp' (LetIn name u v) = LetIn name (elabExp' u) (elabExp' v)

transform :: Exp Funcs Name -> LNExp
transform = go []
 where
  go :: [Name] -> Exp Funcs Name -> LNExp
  go _ (Const xs) = Const xs
  go env (V name) = if name `elem` env then V (Free name) else V (Global name)
  go env (App fs e t) = App fs (go env e) t
  go env (Print e) = Print (go env e)
  go env (LetIn name u v) =
    let u' = go env u
     in LetIn name u' (close name (go (name : env) v))

elabExp :: Exp SFuncs Name -> LNExp
elabExp = transform . elabExp'

elab :: SDecl -> Decl Funcs Var
elab (Decl p name body) = Decl p name (elabExp body)
elab (DeclFunc p name fns) = DeclFunc p name (transformFuncs fns)
