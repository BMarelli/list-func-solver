module Subst where

import Data.List (elemIndex)
import Lang

varChanger ::
  (Int -> Name -> LNExp) -> -- que hacemos con las variables localmente libres
  (Int -> Int -> LNExp) -> -- que hacemos con los indices de De Bruijn
  LNExp ->
  LNExp
varChanger local bound = go 0
 where
  go :: Int -> LNExp -> LNExp
  go n (V (Bound i)) = bound n i
  go n (V (Free x)) = local n x
  go _ (V (Global x)) = V (Global x)
  go n (App fs e t) = App fs (go n e) t
  go _ t@(Const _) = t
  go n (Print e) = Print (go n e)
  go n (LetIn name u v) = LetIn name (go n u) (go (n + 1) v)

openN :: [Name] -> LNExp -> LNExp
openN ns = varChanger (\_ n -> V (Free n)) bnd
 where
  bnd depth i
    | i < depth = V (Bound i)
    | i >= depth && i < depth + nns =
        V (Free (ns !! (i - depth)))
    | otherwise = error "openN: M is not LC"
  nns = length ns

closeN :: [Name] -> LNExp -> LNExp
closeN ns = varChanger lcl (\_ i -> V (Bound i))
 where
  lcl depth y =
    case elemIndex y ns of
      Just i -> V (Bound (i + depth))
      Nothing -> V (Free y)

substN :: [LNExp] -> LNExp -> LNExp
substN ns = varChanger (\_ n -> V (Free n)) bnd
 where
  bnd depth i
    | i < depth = V (Bound i)
    | i >= depth && i < depth + nns = ns !! (i - depth)
    | otherwise = error "substN: M is not LC"
  nns = length ns

subst :: LNExp -> LNExp -> LNExp
subst n = substN [n]

close :: Name -> LNExp -> LNExp
close nm = closeN [nm]

open :: Name -> LNExp -> LNExp
open x = openN [x]
