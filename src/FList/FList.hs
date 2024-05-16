{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FList.FList where

import Data.List.NonEmpty as NonEmpty
import Lang
import MonadFL (MonadFL)

class FList l where
  length :: Num a => l a -> Int
  fromList :: Num a => [a] -> l a
  toList :: Num a => l a -> [a]

  apply :: (MonadFL m, Num a, Eq a) => Funcs -> l a -> m (l a)
  apply (Zero o) l = zero o l
  apply (Succ o) l = succesor o l
  apply (Delete o) l = delete o l
  apply (Rep fns) l = rep fns l
  apply Void l = return l
  apply _ _ = undefined

  zero :: (MonadFL m, Num a) => Orientation -> l a -> m (l a)
  succesor :: (MonadFL m, Num a) => Orientation -> l a -> m (l a)
  delete :: (MonadFL m, Num a) => Orientation -> l a -> m (l a)
  rep :: (MonadFL m, Num a, Eq a) => Seq Funcs -> l a -> m (l a)


applyFuncs :: forall l m a. (FList l, MonadFL m, Num a, Eq a) => Seq Funcs -> l a -> m (l a)
applyFuncs (f :| []) l = apply f l
applyFuncs (f :| fs) l = apply f l >>= applyFuncs (NonEmpty.fromList fs)
