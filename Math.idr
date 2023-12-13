
module Math

import Data.Vect  
import Data.SortedMap  
import Language  

public export
Math : Language Nat
Math = MkLanguage constants (SortedMap.fromList $ map (\c => (c.name, c)) operators)
  where
    constants = the (List Nat) [0, 1]
    binop : String -> (Nat -> Nat -> Nat) -> Operator Nat
    binop nm fn = MkOperator 2 nm $ Just . (ncurry fn)
    eqInt : Nat -> Nat -> Nat
    eqInt m n = if m == n then 1 else 0
    ltInt : Nat -> Nat -> Nat
    ltInt m n = if m < n then 1 else 0
    operators = the (List _) [ binop "+" (+) , binop "<" ltInt , binop "=" eqInt]
