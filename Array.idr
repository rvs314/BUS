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


headOp : {n : Nat} -> Operator (Vect (S n) v) -> Operator v
headOp (MkOperator arity name denotation) = 
       MkOperator arity name (map head . denotation . map (replicate $ S n))

export
head : {n : Nat} -> Term (Vect n v) -> Term v
head {n=0} _ = Symbol "Empty vector?"
head {n=S n} (Symbol str) = Symbol str
head {n=S n} (Constant x) = Constant (head x)
head {n=S n} (Application op@(MkOperator a _ _) xs) = Application (headOp op) (map head xs)

-- head : {n : Nat} -> Language (Vect (S n) v) -> Language v 
-- head (MkLanguage constants rators) = 
--   MkLanguage (map head constants) 
--              (map (\(MkOperator arity name deno) => 
--                     (MkOperator arity name (\as => head <$> deno (map (replicate $ S n) as)))) 
--                     rators)
