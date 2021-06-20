module List.TreeFList where
import List.FList

import AST

data Tree a = Empty | Leaf a | Node Int (Tree a) (Tree a)
              deriving Show

fromRight :: Either a b -> b
fromRight (Right x) = x

instance FList Tree where
  lengthFL Empty = 0
  lengthFL (Leaf _) = 1
  lengthFL (Node n _ _) = n

  fromList [] = Empty
  fromList [x] = Leaf x
  fromList xs = let len = length xs
                    m = div len 2
                    (ys, zs) = splitAt m xs
                in Node len (fromList ys) (fromList zs)

  quote Empty = []
  quote (Leaf x) = [x]
  quote (Node _ l r) = quote l ++ quote r
  
  printFL l = show l

  zero _ Empty = Leaf (Num 0)
  zero L t = Node (lengthFL t + 1) (Leaf (Num 0)) t
  zero R t = Node (lengthFL t + 1) t (Leaf (Num 0))
  
  succesor _ Empty = Left InvalidAplication
  succesor _ (Leaf (Num x)) = Right $ Leaf (Num (succ x))
  succesor L (Node n l r) = let l' = succesor L l
                            in Right $ Node n (fromRight l') r
  succesor R (Node n l r) = let r' = succesor R l
                            in Right $ Node n l (fromRight r')
  succesor _ l = Right  l

  delete _ Empty = Left InvalidAplication
  delete _ (Leaf (Num _)) = Right Empty
  delete L (Node _ l r) = let l' = fromRight $ delete L l
                          in Right $ Node (lengthFL l' + lengthFL r) l' r
  delete R (Node _ l r) = let r' = fromRight $ delete R r
                          in Right $ Node (lengthFL l + lengthFL r') l r'
  delete _ l = Right l

  rep = undefined
  -- rep fn l | lengthFL l >= 2 = rep_ (fn, []) l
  --          | otherwise       = Left InvalidAplication
  --   where
  --     rep_ :: ([Funcs], [Funcs]) -> Tree Elements -> Either Error (Tree Elements)
  --     rep_ _ Empty = Left InvalidAplication
  --     rep_ ([], rst) l@(Node _ (Leaf (Num x)) (Leaf (Num y))) | x == y    = Right l
  --                                                             | otherwise = rep_ (reverse rst, []) l
  --     rep_ ([], rst) l = Right l
  --     rep_ (f:fs, []) l@(Node _ (Leaf (Num x)) (Leaf (Num y))) | x == y    = Right l
  --                                                              | otherwise = case f of
  --                                                                  Zero or -> rep_ (fs, [f]) (zero or l)
  --                                                                  Succ or -> rep_ (fs, [f]) (fromRight (succesor or l))
  --                                                                  Delete or -> rep_ (fs, [f]) (fromRight (delete or l))
  --                                                                  Rep fns -> rep_ (fs, [f]) (fromRight (rep fns l))
  --     rep_ (f:fs, rst) l = case f of
  --                           Zero or -> rep_ (fs, f:rst) (zero or l)
  --                           Succ or -> rep_ (fs, f:rst) (fromRight (succesor or l))
  --                           Delete or -> rep_ (fs, f:rst) (fromRight (delete or l))
  --                           Rep fns -> rep_ (fs, f:rst) (fromRight(rep fns l))
