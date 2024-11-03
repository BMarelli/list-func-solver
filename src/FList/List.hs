{-# OPTIONS_GHC -Wno-orphans #-}

module FList.List where

import FList.FList
import Lang
import MonadFL (MonadFL)
import PPrint (ppError)

instance FList [] where
  length = Prelude.length
  fromList = id
  toList = id

  zero L l = return (0 : l)
  zero R l = return (l ++ [0])

  succesor _ [] = ppError "Empty list"
  succesor L (x : xs) = return (x + 1 : xs)
  succesor R xs = return (init xs ++ [last xs + 1])

  delete _ [] = ppError "Empty list"
  delete L (_ : xs) = return xs
  delete R xs = return (init xs)

  rep fns l
    | FList.FList.length l < 2 = ppError "List too short"
    | otherwise = go fns l
   where
    go :: (MonadFL m, Num a, Eq a) => Seq Funcs -> [a] -> m [a]
    go fns' l'
      | head l' == last l' = return l'
      | otherwise = applyFuncs fns' l' >>= rep fns'
