module List.ListFList where
import List.FList
import AST

fromRight :: Either a b -> b
fromRight (Right x) = x
instance FList [] where
  lengthFL l = length l
  
  fromList l = l

  quote l = l

  zero L l = (Num 0):l
  zero R l = l ++ [Num  0]

  succesor _ [] = Left InvalidAplication 
  succesor L ((Num x):xs) = Right $ (Num (succ x)):xs
  succesor R l = Right $ succ_ l
      where
        succ_ :: [Elements] -> [Elements]
        succ_ [Num x] = [Num (succ x)]
        succ_ (x:xs) = x:(succ_ xs)

  delete _ [] = Left InvalidAplication
  delete L ((Num x):xs) = Right xs
  delete L l@(Generic:xs) = Right l
  delete R l = Right $ del_ l
      where
        del_ :: [Elements] -> [Elements]
        del_ [Generic] = [Generic]
        del_ [Num _] = []
        del_ (x:xs) = x:(del_ xs) 

  rep fn l | length l >= 2 = rep_ (fn, []) l
           | otherwise     = Left InvalidAplication
      where
        getLast :: [Elements] -> Int
        getLast [Num x] = x
        getLast (_:xs) = getLast xs
        rep_ :: ([Funcs], [Funcs]) -> [Elements] -> Either Error [Elements]
        rep_ _ [] = Left InvalidAplication
        rep_ ([], rst) l@((Num x):xs) = if getLast xs == x then Right l
                                        else rep_ (reverse rst, []) l
        rep_ ([], rst) l = rep_ (reverse rst, []) l
        rep_ (f:fs, []) l = case f of
                        Zero or -> rep_ (fs, [f]) (zero or l)
                        Succ or -> rep_ (fs, [f]) (fromRight (succesor or l))
                        Delete or -> rep_ (fs, [f]) (fromRight (delete or l))
                        Rep fns -> rep_ (fs, [f]) (fromRight(rep fns l))
        rep_ (f:fs, rst) l = case f of
                                Zero or -> rep_ (fs, f:rst) (zero or l)
                                Succ or -> rep_ (fs, f:rst) (fromRight (succesor or l))
                                Delete or -> rep_ (fs, f:rst) (fromRight(delete or l))
                                Rep fns -> rep_ (fs, f:rst) (fromRight(rep fns l))

