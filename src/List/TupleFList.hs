module List.TupleFList where

import AST
import List.FList

data TList x = TList [x] [x] deriving (Show)

fromRight :: Either a b -> b
fromRight (Right x) = x

balance :: TList Elements -> TList Elements
balance (TList [] [x]) = TList [x] []
balance (TList xs ys) =
  let list = xs ++ reverse ys
      len = length list
      mid = div len 2
      (xs', ys') = splitAt mid list
   in TList xs' (reverse ys')

-- balance (TList xs []) = fromList xs :: TList Elements
-- balance (TList [] ys) = fromList (reverse ys) :: TList Elements
-- balance l = l

instance FList TList where
  lengthFL (TList xs ys) = length xs + length ys

  fromList list =
    let len = length list
        mid = div len 2
        (xs, ys) = splitAt mid list
     in TList xs (reverse ys)

  quote (TList xs ys) = xs ++ reverse ys

  printFL l = show l

  zero L (TList xs ys) = TList (0 : xs) ys
  zero R (TList xs ys) = TList xs (0 : ys)

  succesor _ (TList [] []) = Left InvalidAplication
  succesor L l@(TList [] ys) = succesor L (balance l)
  succesor L (TList ((x) : xs) ys) = Right $ TList (((succ x)) : xs) ys
  succesor R l@(TList xs []) = succesor R (balance l)
  succesor R (TList xs ((y) : ys)) = Right $ TList xs (((succ y)) : ys)

  delete _ (TList [] []) = Left InvalidAplication
  delete L l@(TList [] ys) = delete L (balance l)
  delete L (TList ((_) : xs) ys) = Right $ TList xs ys
  delete R l@(TList xs []) = delete R (balance l)
  delete R (TList xs ((_) : ys)) = Right $ TList xs ys

  -- succesor _ (TList [] []) = Left InvalidAplication
  -- succesor L (TList [] [ x]) = Right $ TList [] [ (succ x)]
  -- succesor L (TList (( x):xs) ys) = Right $ TList ( (succ x):xs) ys
  -- succesor R (TList xs (( y):ys)) = Right $ TList xs ( (succ y):ys)

  -- delete _ (TList [] []) = Left InvalidAplication
  -- delete L (TList (( _):xs) ys) = Right $ TList xs ys
  -- delete L (TList [] ys) = delete L (TList (reverse ys) [])
  -- delete R (TList xs (( _):ys)) = Right $ TList xs ys
  -- delete R (TList xs []) = delete R (TList [] (reverse xs))
  -- delete _ l = Right l

  rep fn list
    | lengthFL list >= 2 = rep_ (fn, []) list
    | otherwise = Left InvalidAplication
    where
      rep_ :: ([Funcs], [Funcs]) -> TList Elements -> Either Error (TList Elements)
      rep_ _ (TList [] []) = Left InvalidAplication
      rep_ ([], rst) l@(TList ((x) : xs) ((y) : ys))
        | x == y = Right l
        | otherwise = rep_ (reverse rst, []) l
      rep_ ([], rst) l = rep_ (reverse rst, []) (balance l)
      rep_ (f : fs, []) l@(TList ((x) : xs) ((y) : ys))
        | x == y = Right l
        | otherwise = case f of
          Zero or -> rep_ (fs, [f]) (zero or (balance l))
          Succ or -> rep_ (fs, [f]) (fromRight (succesor or (balance l)))
          Delete or -> rep_ (fs, [f]) (fromRight (delete or (balance l)))
          Rep fns -> rep_ (fs, [f]) (fromRight (rep fns (balance l)))
      rep_ (f : fs, rst) l = case f of
        Zero or -> rep_ (fs, f : rst) (zero or (balance l))
        Succ or -> rep_ (fs, f : rst) (fromRight (succesor or (balance l)))
        Delete or -> rep_ (fs, f : rst) (fromRight (delete or (balance l)))
        Rep fns -> rep_ (fs, f : rst) (fromRight (rep fns (balance l)))

-- instance Show a => Show (TList a) where
--   show (TList (xs, ys)) = "[" ++ show_ (length xs) (length ys) xs (reverse ys) 0
--       where
--         show_ lxs lys xs ys i | i == lxs + lys     = "]"
--                               | i == lxs + lys - 1 = show (ys !! (i-lxs)) ++ "]"
--                               | i < lxs           = show (xs !! i) ++ "," ++ show_ lxs lys xs ys (i+1)
--                               | i < lxs + lys     = show (ys !! (i-lxs)) ++ "," ++ show_ lxs lys xs ys (i+1)
--         fromNum ( v) = v
