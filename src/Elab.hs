module Elab (elab, elabExp, transformFuncs, transformPower) where

import Data.List.NonEmpty
import GHC.Base (join)
import Lang
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

elabExp :: Exp SFuncs Name -> Exp Funcs Name
elabExp (Const xs) = Const xs
elabExp (V name) = V name
elabExp (App sfns e ty) = App (transformFuncs sfns) (elabExp e) ty
elabExp (Print e) = Print (elabExp e)

elab :: SDecl -> Decl Funcs Name
elab (Decl p name body) = Decl p name (elabExp body)
elab (DeclFunc p name fns) = DeclFunc p name (transformFuncs fns)
