module Array

import Data.Vect
import Data.SortedMap

import Language

public export
Array : (n : Nat) -> Language v -> Language (Vect n v)
Array k (MkLanguage constants rators) = 
  MkLanguage (map (replicate k) constants)
             (map scatterOp rators)
  where
    scatterOp : Operator val -> Operator (Vect k val)
    scatterOp (MkOperator arity name denotation) = 
      MkOperator arity name (traverse denotation . sequence)


