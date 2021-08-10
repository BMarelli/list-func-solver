module List.SeqFList where

import AST
import Data.Maybe
import Data.Sequence as S hiding (reverse)
import List.FList

fromRight :: Either a b -> b
fromRight (Right x) = x

getLast :: S.Seq Elements -> Int
getLast (xs :|> x) = x

instance FList S.Seq where
  lengthFL = S.length

  fromList = S.fromList

  printFL l = show l

  quote Empty = []
  quote (x :<| xs) = x : quote xs
  quote (xs :|> x) = quote xs ++ [x]

  zero _ Empty = singleton 0
  zero L l = 0 <| l
  zero R l = l |> 0

  succesor _ Empty = Left InvalidAplication
  succesor L (x :<| xs) = Right $ (succ x) <| xs
  succesor R (xs :|> x) = Right $ xs |> (succ x)
  succesor _ xs = Right xs

  delete _ Empty = Left InvalidAplication
  delete L (_ :<| xs) = Right xs
  delete R (xs :|> _) = Right xs
  delete _ xs = Right xs

  rep fn l
    | lengthFL l >= 2 = rep_ (fn, []) l
    | otherwise = Left InvalidAplication
    where
      rep_ :: ([Funcs], [Funcs]) -> S.Seq Elements -> Either Error (S.Seq Elements)
      rep_ _ Empty = Left InvalidAplication
      rep_ ([], rst) l@(x :<| xs)
        | x == getLast xs = Right l
        | otherwise = rep_ (reverse rst, []) l
      rep_ ([], rst) l = rep_ (reverse rst, []) l
      rep_ (f : fs, []) l@(x :<| xs)
        | x == getLast xs = Right l
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
