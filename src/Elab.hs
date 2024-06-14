module Elab (elab, elabExp, transformFuncs, transformPower) where

import Data.List.NonEmpty
import GHC.Base (join)
import Lang
import Prelude hiding (filter, map)

transformPower :: Seq SFuncs -> Seq SFuncs
transformPower (SPower n fns :| [])
  | n == 0 = fromList [SVoid]
  | otherwise = fromList $ join (replicate n (toList (transformPower fns)))
transformPower sfs@(_ :| []) = sfs
transformPower (SPower n fns :| sfns)
  | n == 0 = transformPower (fromList sfns)
  | otherwise =
    fromList $
      (join . replicate n . toList) (transformPower fns)
        <> (toList . transformPower) (fromList sfns)
transformPower (sf :| sfns) = sf :| (toList . transformPower) (fromList sfns)

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

elab :: SDecl -> (Decl Funcs)
elab (Decl p name body) = Decl p name (elabExp body)
elab (DeclFunc p name fns) = DeclFunc p name (transformFuncs fns)
