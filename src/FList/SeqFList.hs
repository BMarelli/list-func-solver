module FList.SeqFList where

import FList.FList
import Lang
import MonadFL (MonadFL, failFL)
import Data.Sequence as S
import Prelude hiding (length)
import qualified Data.Foldable as S hiding (length)

instance FList S.Seq where
  length l = S.length l
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
    | otherwise = rep' fns l
    where
      rep' :: (MonadFL m, Num a, Eq a) => Lang.Seq Funcs -> S.Seq a -> m (S.Seq a)
      rep' fns' l'@(h :<| (_ :|> t))
        | h == t = return l'
        | otherwise = applyFuncs fns' l' >>= rep fns'
      rep' _ _ = failFL "List too short"
