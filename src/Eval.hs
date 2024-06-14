{-# LANGUAGE TypeApplications #-}

module Eval (eval) where

import Data.Functor ((<&>))
import Data.List.NonEmpty as NonEmpty
import qualified FList.FList as FList
import FList.ListFList ()
import FList.CList (CList(..))
import FList.SeqFList ()
import Data.Sequence as S hiding (length, replicate)
import Lang
import MonadFL
import Prelude hiding (map)
import Data.Char

apply :: MonadFL m => Lang.Seq Funcs -> [Element] -> Int -> m [Element]
apply fs l 0 = FList.toList <$> FList.applyFuncs @[] fs l <&> FList.fromList
apply fs l 1 = FList.toList <$> FList.applyFuncs @CList fs (FList.fromList l)
apply fs l 2 = FList.toList <$> FList.applyFuncs @S.Seq fs (FList.fromList l)
apply _ _ _ = failFL "Invalid type"

evalFuncs :: MonadFL m => Lang.Seq Funcs -> m (Lang.Seq Funcs)
evalFuncs ((Rep fs) :| []) = do
  fs' <- evalFuncs fs
  return $ Rep fs' :| []
evalFuncs (Defined name :| []) = do
  fns' <- lookUpFunc name
  case fns' of
    Just fns'' -> evalFuncs fns''
    Nothing -> failFL "Function not found"

evalFuncs (f :| []) = return (f :| [])
evalFuncs ((Rep fs) :| fns) = do
  fs' <- evalFuncs fs
  fns' <- evalFuncs (NonEmpty.fromList fns)
  return $ Rep fs' :| NonEmpty.toList fns'
evalFuncs (Defined name :| fns) = do
  fns' <- lookUpFunc name
  case fns' of
    Just fns'' -> evalFuncs (fns'' <> NonEmpty.fromList fns)
    Nothing -> failFL "Function not found"
evalFuncs (f :| fns) = do
  fns' <- evalFuncs (NonEmpty.fromList fns)
  return $ f :| NonEmpty.toList fns'

eval :: MonadFL m => Exp Funcs -> m [Element]
eval (Const xs) = return xs
eval (V name) = lookUpExp name >>= maybe (failFL "Variable not found") eval
eval (App fs e t) = do
  ty <- lookUpType t
  case ty of
    Just i -> do
      fs' <- evalFuncs fs
      e' <- eval e
      apply fs' e' i
    Nothing -> failFL "Type not found"
eval (Print e) = eval e >>= (\r -> printFL (chr <$> r) >> return r)
eval (Let _ _ _) = failFL "Let not implemented"