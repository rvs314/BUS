
module Math

import Data.Vect  
import Data.SortedMap  
import Language  

public export
Math : Language Integer
Math = MkLanguage constants (SortedMap.fromList $ map (\c => (c.name, c)) operators)
  where
    constants = the (List Integer) [0, 1]
    binop : String -> (Integer -> Integer -> Integer) -> Operator Integer
    binop nm fn = MkOperator 2 nm $ Just . (ncurry fn)
    eqInt : Integer -> Integer -> Integer
    eqInt m n = if m == n then 1 else 0
    ltInt : Integer -> Integer -> Integer
    ltInt m n = if m < n then 1 else 0
    operators = the (List _) [ (MkOperator 1 "-" $ \[k] => Just (-k))
                             , binop "+" (+) , binop "*" (*)
                             , binop "<" ltInt , binop "=" eqInt]
