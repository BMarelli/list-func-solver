{-# OPTIONS_GHC -Wno-orphans #-}

module FList.List where

import FList.FList
import Lang
import MonadFL (MonadFL, failFL)

instance FList [] where
  length = Prelude.length
  fromList = id
  toList = id
  zero L l = return (0 : l)
  zero R l = return (l ++ [0])

  succesor _ [] = failFL "Empty list"
  succesor L (x : xs) = return (x + 1 : xs)
  succesor R xs = return (succ_ xs)
    where
      succ_ [] = undefined
      succ_ [n] = [n + 1]
      succ_ (n : ns) = n : succ_ ns

  delete _ [] = failFL "Empty list"
  delete L (_ : xs) = return xs
  delete R xs = return (init xs)

  rep fns l
    | FList.FList.length l < 2 = failFL "List too short"
    | otherwise = rep' fns l
   where
    rep' :: (MonadFL m, Num a, Eq a) => Seq Funcs -> [a] -> m [a]
    rep' fns' l'
      | head l' == last l' = return l'
      | otherwise = applyFuncs fns' l' >>= rep fns'
