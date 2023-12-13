module Dan

import Language
import Array
import Folds
import Bus
import Data.Fuel
import Data.Vect
import Data.List
import Data.HVect
import Data.Nat
import Data.SortedMap.Dependent
import Data.SortedMap
import Decidable.Equality

asVect : (ks : List a) -> Vect (length ks) a
asVect [] = []
asVect (x :: xs) = x :: asVect xs

unmap : {k : Nat} -> SortedDMap (Fin k) (const a) -> Maybe (Vect k a)
unmap x = sequence $ foldl (\c, (k ** v) => Vect.replaceAt k (Just v) c) (replicate k Nothing) x

export
dan : {v : Type} -> Ord v => 
               (f : Fuel) -> (l : Language v) ->
               List (v, v) -> (w : View v) -> 
               Maybe (Vect w.numKinds (Term v))
dan f l xs w = 
  do let s = splitByKind xs 
     guard $ length (keys s) == w.numKinds
     s' <- traverse (Prelude.traverse (bitraverse (Prelude.traverse (\k => List.lookup k xs)) pure)) s 
     cs <- traverse (\exs => let (ins, outs) = mapFst transpose $ unzip $ asVect exs  in
                             Array.head <$> bus f (Array (length exs) l) (splitVars ins) outs) s'
     unmap cs
  where
    unmap : {k : Nat} -> SortedDMap (Fin k) (const a) -> Maybe (Vect k a)
    unmap x = sequence $ foldl (\c, (k ** v) => Vect.replaceAt k (Just v) c) (replicate k Nothing) x

    splitVars : Vect n t -> Env t
    splitVars = rec 0
      where
        rec : {0 n : Nat} -> Nat -> Vect n t -> Env t
        rec n [] = SortedMap.empty
        rec n (x :: xs) = insert "arg\{show n}" x $ rec (S n) xs
    
    updateOr : DecEq k => {f : k -> Type} -> (i : k) -> (f i -> f i) -> f i -> SortedDMap k f -> SortedDMap k f
    updateOr i up def mp = case lookupPrecise i mp of
                               Nothing => insert i def mp
                               Just o  => insert i (up o) mp

    splitByKind : List (v, v) -> (SortedDMap (Fin w.numKinds) (\k => List (Vect (index k w.kinds) v, v)))
    splitByKind []        = empty
    splitByKind ((i, o) :: ys) with (w.break i)
      splitByKind ((i, o) :: ys) | ((fst ** snd)) = 
        updateOr fst (\k => (snd, o) :: k) [(snd, o)] (splitByKind ys)
