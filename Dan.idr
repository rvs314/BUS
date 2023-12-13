module Dan

import Language
import Folds
import Bus
import Data.Fuel
import Data.Vect
import Data.HVect
import Data.Nat
import Data.SortedMap.Dependent
import Decidable.Equality

natOfFin : (n : Nat) -> Fin (S n)
natOfFin 0 = FZ
natOfFin (S k) = FS $ natOfFin k

data FinVect : Fin j -> Type -> Type where
  Nil : FinVect FZ a
  (::) : a -> FinVect n a -> FinVect (FS n) a

natToFinPlus : (k : Nat) -> (j : Nat) -> Fin (S (k + j))
natToFinPlus 0     j = FZ
natToFinPlus (S k) j = FS (natToFinPlus k j)

dan : {v : Type} -> Ord v => 
               (f : Fuel) -> (l : Language v) ->
               List (v, v) -> (w : View v) -> 
               Maybe (Vect w.numKinds (Term v))
dan f l xs w = 
    let s = splitByKind xs in ?zzzz
  where
    updateOr : DecEq k => {f : k -> Type} -> (i : k) -> (f i -> f i) -> f i -> SortedDMap k f -> SortedDMap k f
    updateOr i up def mp = case lookupPrecise i mp of
                               Nothing => insert i def mp
                               Just o  => insert i (up o) mp

    splitByKind : List (v, v) -> (SortedDMap (Fin w.numKinds) (\k => List (Vect (index k w.kinds) v, v)))
    splitByKind []        = empty
    splitByKind ((i, o) :: ys) with (w.break i)
      splitByKind ((i, o) :: ys) | ((fst ** snd)) = 
        updateOr fst (\k => (snd, o) :: k) [(snd, o)] (splitByKind ys)

