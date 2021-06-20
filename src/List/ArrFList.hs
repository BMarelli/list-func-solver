module List.ArrFList where
import List.FList
import AST
import qualified Data.Vector as V
import Prelude hiding (length)

nth :: V.Vector a -> Int -> a
nth l i = l V.! i

instance FList V.Vector where
  lengthFL arr = V.length arr

  fromList = V.fromList

  quote arr = case lengthFL arr of
                    0 -> []
                    1 -> [nth arr 0]
                    _ -> nth arr 0 : quote (V.tail arr)

  zero = undefined 
  delete = undefined 
  succesor = undefined
  rep = undefined
