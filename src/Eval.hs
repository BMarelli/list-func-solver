{-# LANGUAGE TypeApplications #-}

module Eval (eval) where

import Data.Char
import Data.Functor ((<&>))
import Data.List.NonEmpty as NonEmpty
import Data.Semigroup (Semigroup (sconcat))
import qualified Data.Sequence as S hiding (length, replicate)
import FList.CList (CList (..))
import qualified FList.FList as FList
import FList.List ()
import FList.Sequence ()
import Lang
import MonadFL
import Subst (subst)
import Prelude hiding (map)

apply :: (MonadFL m) => Seq Funcs -> [Element] -> Int -> m [Element]
apply fs l 0 = FList.toList <$> FList.applyFuncs @[] fs l <&> FList.fromList
apply fs l 1 = FList.toList <$> FList.applyFuncs @CList fs (FList.fromList l)
apply fs l 2 = FList.toList <$> FList.applyFuncs @S.Seq fs (FList.fromList l)
apply _ _ _ = failFL "Invalid type"

evalFuncs :: (MonadFL m) => Seq Funcs -> m (Seq Funcs)
evalFuncs fns = sconcat <$> mapM go fns
 where
  go :: (MonadFL m) => Funcs -> m (Seq Funcs)
  go (Rep fs) = NonEmpty.singleton . Rep <$> evalFuncs fs
  go (Defined name) = lookUpFunc name >>= maybe (failFL ("Function not found: " ++ name)) evalFuncs
  go f = return . NonEmpty.singleton $ f

eval :: (MonadFL m) => Exp Funcs Var -> m [Element]
eval (Const xs) = return xs
eval (V var) =
  case var of
    Global name -> lookUpExp name >>= maybe (failFL ("Variable not found: " ++ name)) eval
    _ -> failFL "error: free variable"
eval (App fs e t) = do
  ty <- lookUpType t
  case ty of
    Just i -> do
      fs' <- evalFuncs fs
      e' <- eval e
      apply fs' e' i
    Nothing -> failFL ("Type not found: " ++ show t)
eval (Print e) = eval e >>= (\r -> printFL (chr <$> r) >> return r)
eval (LetIn _ u v) = do
  u' <- eval u
  eval (subst (Const u') v)
