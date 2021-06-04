module List.FList where
import AST

class FList l where
  fromList :: ListElements -> l Elements
  lengthFL :: l a -> Int
  quote :: l Elements -> ListElements

  zero :: Orientation -> l Elements -> l Elements
  succesor :: Orientation -> l Elements -> l Elements
  delete :: Orientation -> l Elements -> l Elements
  rep :: [Funcs] -> l Elements -> l Elements

