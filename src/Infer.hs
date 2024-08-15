module Infer (infer) where

import Data.Foldable (foldlM)
import Lang
import MonadFL (MonadFL, failFL, lookUpExp, lookUpFunc)
import PPrint (ppError)
import Subst (subst)

infer :: (MonadFL m) => Int -> Exp Funcs Var -> m Int
infer i (Const l) = return (i + length l)
infer i (V var) =
  case var of
    Global name -> lookUpExp name >>= maybe (ppError ("Variable not found: " ++ name)) (infer i)
    _ -> failFL "error: free variable"
infer i (App fs e _) = foldlM inferFunc i fs >>= (`infer` e)
infer i (Print e) = infer i e
infer i (LetIn _ u v) = infer i (subst u v)

inferFunc :: (MonadFL m) => Int -> Funcs -> m Int
inferFunc i (Zero _) = return (i + 1)
inferFunc i (Succ _) = return i
inferFunc i (Delete _) = return (i - 1)
inferFunc _ (Rep _) = ppError "Rep not allowed in infer"
inferFunc i (Defined name) = lookUpFunc name >>= maybe (ppError ("Function not found: " ++ name)) (foldlM inferFunc i)
inferFunc i Void = return i
