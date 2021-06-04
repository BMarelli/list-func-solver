{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module List.TupleFList where
import List.FList
import AST

newtype TList a = TList ([a], [a]) deriving Show

instance FList TList where
  lengthFL (TList (xs, ys)) = length xs + length ys

  fromList list = let len = length list
                      mid = div len 2
                      (xs, ys) = splitAt mid list
                  in TList (xs, reverse ys)
  
  quote (TList (xs, ys)) = xs ++ reverse ys

  zero L (TList (xs, ys)) = TList (Num 0:xs, ys)
  zero R (TList (xs, ys)) = TList (xs, Num 0:ys)

  succesor L (TList ((Num x):xs, ys)) = TList (Num (succ x):xs, ys)
  succesor R (TList (xs, (Num y):ys)) = TList (xs, Num (succ y):ys)
  succesor _ l = l 

  -- delete L (TList ((Num _):xs, ys)) = TList (xs, ys)
  -- delete L (TList ([], ys)) = delete R (TList (reverse ys, []))
  -- delete R (TList ([], (Num _):ys)) = TList ([], ys)
  -- delete R (TList (xs, (Num _):ys)) = TList (xs, ys)
  -- delete _ (TList (xs, ys)) = TList (xs, ys)

  delete L (TList ((Num _):xs, ys)) = TList (xs, ys)
  delete R (TList ([Num _], [])) = TList ([], [])
  delete _ (TList ([], [])) = TList ([], [])
  delete L (TList ([], ys)) = delete L (TList (reverse ys, []))
  delete R (TList (xs, (Num _):ys)) = TList (xs, ys)
  delete _ l@(TList (xs, ys)) = l

  rep fn l = rep_ (fn, []) l
    where
      rep_ :: ([Funcs], [Funcs]) -> TList Elements -> TList Elements
      rep_ _ l@(TList ([], [])) = l
      rep_ ([], rst) l@(TList ((Num x):xs, (Num y):ys)) | x == y    = l
                                                        | otherwise = rep_ (reverse rst, []) l
      rep_ ([], rst) l@(TList (xs, ys)) = rep_ (reverse rst, []) l
      rep_ (f:fs, []) l@(TList ((Num x):xs, (Num y):ys)) | x == y    = l
                                                         | otherwise = case f of
                                                                          Zero or -> rep_ (fs, [f]) (zero or l)
                                                                          Succ or -> rep_ (fs, [f]) (succesor or l)
                                                                          Delete or -> rep_ (fs, [f]) (delete or l)
                                                                          Rep fns -> rep_ (fs, [f]) (rep fns l)
      rep_ (f:fs, rst) l@(TList (xs, ys)) = case f of
                                              Zero or -> rep_ (fs, f:rst) (zero or l)
                                              Succ or -> rep_ (fs, f:rst) (succesor or l)
                                              Delete or -> rep_ (fs, f:rst) (delete or l)
                                              Rep fns -> rep_ (fs, f:rst) (rep fns l)                                        

-- instance Show a => Show (TList a) where
--   show (TList (xs, ys)) = "[" ++ show_ (length xs) (length ys) xs (reverse ys) 0
--       where
--         show_ lxs lys xs ys i | i == lxs + lys     = "]"
--                               | i == lxs + lys - 1 = show (ys !! (i-lxs)) ++ "]"
--                               | i < lxs           = show (xs !! i) ++ "," ++ show_ lxs lys xs ys (i+1)
--                               | i < lxs + lys     = show (ys !! (i-lxs)) ++ "," ++ show_ lxs lys xs ys (i+1)
--         fromNum (Num v) = v
