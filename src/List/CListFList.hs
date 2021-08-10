module List.CListFList where

import AST
import List.FList

data CList x = Empty | CUnit x | Consnoc x (CList x) x
  deriving (Show)

getLast :: [a] -> (a, [a])
getLast xs = getLast' xs []
  where
    getLast' :: [a] -> [a] -> (a, [a])
    getLast' [x] ys = (x, ys)
    getLast' (x : xs) ys = getLast' xs (x : ys)

fromRight :: Either a b -> b
fromRight (Right x) = x

-- =============================================== Funciones de CList ==================================================
consCL :: a -> CList a -> CList a
consCL v Empty = CUnit v
consCL v (CUnit x) = Consnoc v Empty x
consCL v (Consnoc hd xs lst) = Consnoc v (consCL hd xs) lst

snocCL :: a -> CList a -> CList a
snocCL v Empty = CUnit v
snocCL v (CUnit x) = Consnoc x Empty v
snocCL v (Consnoc hd xs lst) = Consnoc hd (snocCL lst xs) v

headCL :: CList a -> a
headCL (CUnit v) = v
headCL (Consnoc v _ _) = v

tailCL :: CList a -> CList a
tailCL (CUnit v) = Empty
tailCL (Consnoc _ xs v) = case xs of
  Empty -> CUnit v
  CUnit x -> Consnoc x Empty v
  _ -> Consnoc (headCL xs) (tailCL xs) v

lastCL :: CList a -> a
lastCL (CUnit v) = v
lastCL (Consnoc _ _ y) = y

delHeadCL :: CList a -> CList a
delHeadCL (Consnoc _ Empty y) = CUnit y
delHeadCL (Consnoc _ xs y) = Consnoc (headCL xs) (tailCL xs) y
delHeadCL _ = Empty

delLastCL :: CList a -> CList a
delLastCL (Consnoc x xs y) = case xs of
  Empty -> CUnit x
  CUnit v -> Consnoc x Empty v
  _ -> Consnoc x (delLastCL xs) (lastCL xs)

-- =====================================================================================================================

instance FList CList where
  lengthFL Empty = 0
  lengthFL (CUnit _) = 1
  lengthFL (Consnoc _ xs _) = 2 + lengthFL xs

  fromList [] = Empty
  fromList [x] = CUnit x
  fromList (x : xs) = consCL x (fromList xs)

  quote Empty = []
  quote (CUnit x) = [x]
  quote (Consnoc x xs y) = x : (quote xs ++ [y])

  printFL l = show l

  zero _ Empty = CUnit 0
  zero L (CUnit x) = Consnoc 0 Empty x
  zero L l = consCL 0 l
  zero R (CUnit x) = Consnoc x Empty 0
  zero R l = snocCL 0 l

  succesor _ Empty = Left InvalidAplication
  succesor L (CUnit x) = Right $ CUnit (succ x)
  succesor L (Consnoc x xs y) = Right $ Consnoc (succ x) xs y
  succesor R (CUnit x) = Right $ CUnit (succ x)
  succesor R (Consnoc x xs y) = Right $ Consnoc x xs (succ y)

  delete _ Empty = Left InvalidAplication
  delete L (CUnit _) = Right Empty
  delete L l@(Consnoc _ xs y) = Right $ delHeadCL l
  delete R (CUnit _) = Right Empty
  delete R l@(Consnoc _ xs y) = Right $ delLastCL l

  rep fn l
    | lengthFL l >= 2 = rep_ (fn, []) l
    | otherwise = Left InvalidAplication
    where
      rep_ :: ([Funcs], [Funcs]) -> CList Elements -> Either Error (CList Elements)
      rep_ _ Empty = Left InvalidAplication
      rep_ ([], rst) l@(Consnoc x _ y)
        | x == y = Right l
        | otherwise = rep_ (reverse rst, []) l
      rep_ ([], rst) l = rep_ (reverse rst, []) l
      rep_ (f : fs, []) l@(Consnoc x _ y)
        | x == y = Right l
        | otherwise = case f of
          Zero or -> rep_ (fs, [f]) (zero or l)
          Succ or -> rep_ (fs, [f]) (fromRight (succesor or l))
          Delete or -> rep_ (fs, [f]) (fromRight (delete or l))
          Rep fns -> rep_ (fs, [f]) (fromRight (rep fns l))
      rep_ (f : fs, rst) l = case f of
        Zero or -> rep_ (fs, f : rst) (zero or l)
        Succ or -> rep_ (fs, f : rst) (fromRight (succesor or l))
        Delete or -> rep_ (fs, f : rst) (fromRight (delete or l))
        Rep fns -> rep_ (fs, f : rst) (fromRight (rep fns l))
