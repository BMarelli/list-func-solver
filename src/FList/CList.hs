module FList.CList where

import FList.FList
import Lang
import MonadFL (MonadFL, failFL)
import Prelude hiding (length)

data CList a = Empty | CUnit a | Consnoc a (CList a) a

instance Functor CList where
  fmap _ Empty = Empty
  fmap f (CUnit x) = CUnit (f x)
  fmap f (Consnoc h xs l) = Consnoc (f h) (fmap f xs) (f l)

consCL :: a -> CList a -> CList a
consCL v Empty = CUnit v
consCL v (CUnit x) = Consnoc v Empty x
consCL v (Consnoc h xs l) = Consnoc v (consCL h xs) l

snocCL :: a -> CList a -> CList a
snocCL v Empty = CUnit v
snocCL v (CUnit x) = Consnoc x Empty v
snocCL v (Consnoc h xs l) = Consnoc h (snocCL l xs) v

headCL :: CList a -> a
headCL Empty = error "Empty list"
headCL (CUnit v) = v
headCL (Consnoc h _ _) = h

tailCL :: CList a -> CList a
tailCL Empty = error "Empty list"
tailCL (CUnit _) = Empty
tailCL (Consnoc _ xs l) = case xs of
  Empty -> CUnit l
  CUnit v -> Consnoc v Empty l
  _ -> Consnoc (headCL xs) (tailCL xs) l

lastCL :: CList a -> a
lastCL Empty = error "Empty list"
lastCL (CUnit v) = v
lastCL (Consnoc _ _ l) = l

delHeadCL :: CList a -> CList a
delHeadCL (Consnoc _ Empty l) = CUnit l
delHeadCL (Consnoc _ xs l) = Consnoc (headCL xs) (tailCL xs) l
delHeadCL _ = Empty

delLastCL :: CList a -> CList a
delLastCL (Consnoc h xs _) = case xs of
  Empty -> CUnit h
  CUnit v -> Consnoc h Empty v
  _ -> Consnoc h (delLastCL xs) (lastCL xs)
delLastCL _ = Empty

instance FList CList where
  length Empty = 0
  length (CUnit _) = 1
  length (Consnoc _ l _) = 2 + length l

  fromList [] = Empty
  fromList [x] = CUnit x
  fromList (x : xs) = consCL x (fromList xs)

  toList Empty = []
  toList (CUnit x) = [x]
  toList (Consnoc x xs y) = x : toList xs ++ [y]

  zero L l = return $ consCL 0 l
  zero R l = return $ snocCL 0 l

  succesor _ Empty = failFL "Empty list"
  succesor _ (CUnit x) = return $ CUnit (x + 1)
  succesor L (Consnoc h xs l) = return $ Consnoc (h + 1) xs l
  succesor R (Consnoc h xs l) = return $ Consnoc h xs (l + 1)

  delete _ Empty = failFL "Empty list"
  delete _ (CUnit _) = return Empty
  delete L l@Consnoc{} = return $ delHeadCL l
  delete R l@Consnoc{} = return $ delLastCL l

  rep fns l
    | FList.FList.length l < 2 = failFL "List too short"
    | otherwise = rep' fns l
   where
    rep' :: (MonadFL m, Num a, Eq a) => Seq Funcs -> CList a -> m (CList a)
    rep' fns' l'
      | headCL l' == lastCL l' = return l'
      | otherwise = applyFuncs fns' l' >>= rep fns'
