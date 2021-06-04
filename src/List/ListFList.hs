module List.ListFList where
import List.FList
import AST

instance FList [] where
  lengthFL l = length l
  
  fromList l = l

  quote l = l

  zero L l = (Num 0):l
  zero R l = l ++ [Num  0]

  succesor L ((Num x):xs) = (Num (succ x)):xs
  succesor R l = succ_ l
      where
        succ_ :: [Elements] -> [Elements]
        succ_ [Num x] = [Num (succ x)]
        succ_ (x:xs) = x:(succ_ xs)

  delete _ [] = []
  delete L ((Num x):xs) = xs
  delete L l = l
  delete R l = del_ l
      where
        del_ :: [Elements] -> [Elements]
        del_ [Generic] = [Generic]
        del_ [Num _] = []
        del_ (x:xs) = x:(del_ xs) 

  rep fn l = rep_ (fn, []) l
      where
        getLast :: [Elements] -> Int
        getLast [Num x] = x
        getLast (_:xs) = getLast xs
        rep_ :: ([Funcs], [Funcs]) -> [Elements] -> [Elements]
        rep_ _ [] = []
        rep_ ([], rst) l@((Num x):xs) = if getLast xs == x then l
                                        else rep_ (reverse rst, []) l
        rep_ ([], rst) l = rep_ (reverse rst, []) l
        rep_ (f:fs, []) l = case f of
                        Zero or -> rep_ (fs, [f]) (zero or l)
                        Succ or -> rep_ (fs, [f]) (succesor or l)
                        Delete or -> rep_ (fs, [f]) (delete or l)
                        Rep fns -> rep_ (fs, [f]) (rep fns l)
        rep_ (f:fs, rst) l = case f of
                                Zero or -> rep_ (fs, f:rst) (zero or l)
                                Succ or -> rep_ (fs, f:rst) (succesor or l)
                                Delete or -> rep_ (fs, f:rst) (delete or l)
                                Rep fns -> rep_ (fs, f:rst) (rep fns l)
