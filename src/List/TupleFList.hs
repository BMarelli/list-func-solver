module List.TupleFList where
import List.FList
import AST

data TList x = TList [x] [x] deriving Show

fromRight :: Either a b -> b
fromRight (Right x) = x

balance :: TList Elements -> TList Elements
balance (TList xs []) = fromList xs :: TList Elements
balance (TList [] ys) = fromList ys :: TList Elements

instance FList TList where
  lengthFL (TList xs ys) = length xs + length ys

  fromList list = let len = length list
                      mid = div len 2
                      (xs, ys) = splitAt mid list
                  in TList xs (reverse ys)
  
  quote (TList xs ys) = xs ++ reverse ys

  zero L (TList xs ys) = TList (Num 0:xs) ys
  zero R (TList xs ys) = TList xs (Num 0:ys)

  succesor _ (TList [] []) = Left InvalidAplication
  succesor L l@(TList [] ys) = succesor L (balance l)
  succesor L (TList ((Num x):xs) ys) = Right $ TList ((Num (succ x)):xs) ys
  succesor R l@(TList xs []) = succesor R (balance l)
  succesor R (TList xs ((Num y):ys)) = Right $ TList xs ((Num (succ y)):ys)
  succesor _ l@(TList xs ys) = Right l

  delete _ (TList [] []) = Left InvalidAplication
  delete L l@(TList [] ys) = delete L (balance l)
  delete L (TList ((Num _):xs) ys) = Right $ TList xs ys
  delete R l@(TList xs []) = delete R (balance l)
  delete R (TList xs ((Num _):ys)) = Right $ TList xs ys
  delete _ l@(TList xs ys) = Right l

  -- succesor _ (TList [] []) = Left InvalidAplication
  -- succesor L (TList [] [Num x]) = Right $ TList [] [Num (succ x)]
  -- succesor L (TList ((Num x):xs) ys) = Right $ TList (Num (succ x):xs) ys
  -- succesor R (TList xs ((Num y):ys)) = Right $ TList xs (Num (succ y):ys)

  -- delete _ (TList [] []) = Left InvalidAplication
  -- delete L (TList ((Num _):xs) ys) = Right $ TList xs ys
  -- delete L (TList [] ys) = delete L (TList (reverse ys) [])
  -- delete R (TList xs ((Num _):ys)) = Right $ TList xs ys
  -- delete R (TList xs []) = delete R (TList [] (reverse xs))
  -- delete _ l = Right l

  rep fn list | lengthFL list >= 2 = rep_ (fn, []) list
              | otherwise          = Left InvalidAplication
    where
      rep_ :: ([Funcs], [Funcs]) -> TList Elements -> Either Error (TList Elements)
      rep_ _ (TList [] []) = Left InvalidAplication
      rep_ ([], rst) l@(TList ((Num x):xs) ((Num y):ys)) | x == y    = Right l
                                                         | otherwise = rep_ (reverse rst, []) l
      rep_ ([], rst) l = rep_ (reverse rst, []) (balance l)
      rep_ (f:fs, []) l@(TList ((Num x):xs) ((Num y):ys)) | x == y    = Right l
                                                          | otherwise = case f of
                                                                       Zero or -> rep_ (fs, [f]) (zero or l)
                                                                       Succ or -> rep_ (fs, [f]) (fromRight (succesor or l))
                                                                       Delete or -> rep_ (fs, [f]) (fromRight (delete or l))
                                                                       Rep fns -> rep_ (fs, [f]) (fromRight (rep fns l))
      rep_ (f:fs, rst) l = case f of
                              Zero or -> rep_ (fs, f:rst) (zero or l)
                              Succ or -> rep_ (fs, f:rst) (fromRight (succesor or l))
                              Delete or -> rep_ (fs, f:rst) (fromRight (delete or l))
                              Rep fns -> rep_ (fs, f:rst) (fromRight (rep fns l))

-- instance Show a => Show (TList a) where
--   show (TList (xs, ys)) = "[" ++ show_ (length xs) (length ys) xs (reverse ys) 0
--       where
--         show_ lxs lys xs ys i | i == lxs + lys     = "]"
--                               | i == lxs + lys - 1 = show (ys !! (i-lxs)) ++ "]"
--                               | i < lxs           = show (xs !! i) ++ "," ++ show_ lxs lys xs ys (i+1)
--                               | i < lxs + lys     = show (ys !! (i-lxs)) ++ "," ++ show_ lxs lys xs ys (i+1)
--         fromNum (Num v) = v
