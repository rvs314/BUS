
module Math

import Data.Vect  
import Data.SortedMap  
import Language  

public export
Math : Language
Math = MkLanguage Integer constants (SortedMap.fromList operators)
  where
    constants = the (List Integer) [0, 1]
    binop : String -> (Integer -> Integer -> Integer) -> (String, (DPair Nat (Operator Integer)))
    binop nm fn = (nm, (2 ** MkOperator nm $ Just . (ncurry fn)))
    eqInt : Integer -> Integer -> Integer
    eqInt m n = if m == n then 1 else 0
    ltInt : Integer -> Integer -> Integer
    ltInt m n = if m < n then 1 else 0
    operators = the (List _) [ ("-", (1 ** (MkOperator "-" $ \[k] => Just (-k))))
                             , binop "+" (+) , binop "*" (*)
                             , binop "<" ltInt , binop "=" eqInt]
