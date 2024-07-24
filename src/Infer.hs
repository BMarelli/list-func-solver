module Infer (infer) where

import Data.Foldable (foldlM)
import Lang
import MonadFL (MonadFL, failFL, lookUpExp, lookUpFunc)

infer :: (MonadFL m) => Int -> Exp Funcs -> m Int
infer i (Const l) = return (i + length l)
infer i (V name) = lookUpExp name >>= maybe (failFL "Variable not found") (infer i)
infer i (App fs e _) = foldlM inferFunc i fs >>= (`infer` e)
infer i (Print e) = infer i e

inferFunc :: (MonadFL m) => Int -> Funcs -> m Int
inferFunc i (Zero _) = return (i + 1)
inferFunc i (Succ _) = return i
inferFunc i (Delete _) = return (i - 1)
inferFunc _ (Rep _) = failFL "Rep not allowed in infer"
inferFunc i (Defined name) = lookUpFunc name >>= maybe (failFL "Function not found") (foldlM inferFunc i)
inferFunc i Void = return i
