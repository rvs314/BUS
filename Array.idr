module Array

import Data.Vect
import Data.SortedMap
import Math

import Language

public export
Array : Nat -> Language -> Language
Array k (MkLanguage val constants rators) = 
  MkLanguage (Vect k val) 
             (map (replicate k) constants)
             (map (\(n ** op) => (n ** scatterOp n op)) rators)
  where
    scatterOp : (n : Nat) -> Operator val n -> Operator (Vect k val) n
    scatterOp n (MkOperator name denotation) = 
      MkOperator name (traverse denotation . sequence)


