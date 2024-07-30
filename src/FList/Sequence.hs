{-# OPTIONS_GHC -Wno-orphans #-}

module FList.Sequence where

import qualified Data.Foldable as S hiding (length)
import Data.Sequence as S
import FList.FList
import Lang
import MonadFL (MonadFL, failFL)
import Prelude hiding (length)

instance FList S.Seq where
  length = S.length
  fromList = S.fromList
  toList = S.toList

  zero L l = return (0 <| l)
  zero R l = return (l |> 0)

  succesor _ Empty = failFL "Empty list"
  succesor L (n :<| l) = return $ (n + 1) :<| l
  succesor R (l :|> n) = return $ l :|> (n + 1)

  delete _ Empty = failFL "Empty list"
  delete L (_ :<| l) = return l
  delete R (l :|> _) = return l

  rep fns l
    | FList.FList.length l < 2 = failFL "List too short"
    | otherwise = go fns l
   where
    go :: (MonadFL m, Num a, Eq a) => Lang.Seq Funcs -> S.Seq a -> m (S.Seq a)
    go fns' l'@(h :<| (_ :|> t))
      | h == t = return l'
      | otherwise = applyFuncs fns' l' >>= rep fns'
    go _ _ = failFL "List too short"
