module List.ListFList where

import AST
import List.FList

fromRight :: Either a b -> b
fromRight (Right x) = x

instance FList [] where
  lengthFL l = length l

  fromList l = l

  quote l = l

  printFL l = show l

  zero L l = 0 : l
  zero R l = l ++ [0]

  succesor _ [] = Left InvalidAplication
  succesor L (x : xs) = Right $ (succ x) : xs
  succesor R l = Right $ succ_ l
    where
      succ_ :: [Elements] -> [Elements]
      succ_ [x] = [(succ x)]
      succ_ (x : xs) = x : (succ_ xs)

  delete _ [] = Left InvalidAplication
  delete L (x : xs) = Right xs
  delete R l = Right $ del_ l
    where
      del_ :: [Elements] -> [Elements]
      del_ [_] = []
      del_ (x : xs) = x : (del_ xs)

  rep fn l
    | length l >= 2 = rep_ (fn, []) l
    | otherwise = Left InvalidAplication
    where
      getLast :: [Elements] -> Int
      getLast [v] = v
      getLast (_ : ys) = getLast ys
      rep_ :: ([Funcs], [Funcs]) -> [Elements] -> Either Error [Elements]
      rep_ _ [] = Left InvalidAplication
      rep_ ([], rst) l@(x : xs)
        | getLast xs == x = Right l
        | otherwise = rep_ (reverse rst, []) l
      rep_ ([], rst) l = rep_ (reverse rst, []) l
      rep_ (f : fs, []) l@(x : xs)
        | getLast xs == x = Right l
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
