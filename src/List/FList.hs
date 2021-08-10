module List.FList where

import AST

-- class Monad m => FList m l where
-- monadizar esto

class FList l where
  lengthFL :: l a -> Int
  fromList :: ListElements -> l Elements
  quote :: l Elements -> ListElements
  printFL :: l Elements -> String

  zero :: Orientation -> l Elements -> l Elements
  succesor :: Orientation -> l Elements -> Either Error (l Elements)
  delete :: Orientation -> l Elements -> Either Error (l Elements)
  rep :: [Funcs] -> l Elements -> Either Error (l Elements)
