module Elab (elab, elabExp, transformFuncs, transformPower) where

import Data.List.NonEmpty
import GHC.Base (join)
import Lang
import Prelude hiding (filter, map)

transformPower :: Seq SFuncs -> Seq SFuncs
transformPower = fromList . concatMap go
 where
  go :: SFuncs -> [SFuncs]
  go (SPower n fs) = join . replicate n . toList . transformPower $ fs
  go sf = pure sf

transformFunc :: SFuncs -> Funcs
transformFunc (SZero o) = Zero o
transformFunc (SSucc o) = Succ o
transformFunc (SDelete o) = Delete o
transformFunc (SRep sfns) = Rep (transformFuncs sfns)
transformFunc (SDefined name) = Defined name
transformFunc SVoid = Void
transformFunc _ = undefined

transformFuncs :: Seq SFuncs -> Seq Funcs
transformFuncs = map transformFunc . transformPower

elabExp :: Exp SFuncs -> Exp Funcs
elabExp (Const xs) = Const xs
elabExp (V name) = V name
elabExp (App sfns e ty) = App (transformFuncs sfns) (elabExp e) ty
elabExp (Print e) = Print (elabExp e)

elab :: SDecl -> Decl Funcs
elab (Decl p name body) = Decl p name (elabExp body)
elab (DeclFunc p name fns) = DeclFunc p name (transformFuncs fns)
