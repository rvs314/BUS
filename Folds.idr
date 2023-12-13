module Folds 

import Language
import Bus
import Data.Fin
import Data.Vect
import Data.List
import Data.Fun
import Data.HVect


public export
record View (a : Type) where
  constructor MkView 
  numKinds : Nat
  kinds : Vect numKinds Nat
  break : a -> (n : Fin numKinds ** Vect (index n kinds) a)
  build : (n : Fin numKinds) -> Vect (index n kinds) a -> a 

public export
natView : View Nat
natView = MkView { numKinds = 2, kinds = [0, 1], break , build }
  where
    break : Nat -> (n : Fin 2 ** Vect (index n [0, 1]) Nat)
    break 0     = (0 ** [])
    break (S k) = (1 ** [k])

    build : (n : Fin 2) -> Vect (index n [0, 1]) Nat -> Nat
    build FZ      ks  = 0
    build (FS FZ) [x] = S x

mapIndex : {k : Fin j} -> {ks : Vect j a} -> index k (map f ks) = f (index k ks)
mapIndex {k = FZ} {ks = (x :: xs)} = Refl
mapIndex {k = (FS y)} {ks = (x :: xs)} = mapIndex

public export
foldView : (v : View a) -> HVect (map (flip Language.nary a) v.kinds) -> a -> a
foldView v xs y = 
  let (n ** as) = v.break y
      as = map (foldView v xs) as
      r : nary (index n v.kinds) a
      r = rewrite sym $ mapIndex {f=(\y => nary y a)} {k=n} {ks=v.kinds} 
          in index n xs
   in ncurry r as

double : Nat -> Nat
double = foldView natView [ 0 , S . S ]
