module Dan 

import Language
import Bus
import Data.Fin
import Data.Vect
import Data.List
import Data.Fun

nary : Nat -> Type -> Type
nary k x = ?nary_rhs

-- record View (a : Type) where
--   constructor MkView 
--   kinds : List Nat
--   break : a -> (id : Fin (length kinds) ** Vect (length (index' )

-- natView : View Nat
-- natView = MkView { kinds = [0, 1] , break , build }
--   where
--     break : Nat -> (n : Fin 2 ** Vect (index' [0, 1] n) Nat)
--     break 0     = (0 ** [])
--     break (S k) = (1 ** [k])

--     build : (n : Fin 2) -> Vect (index' [0, 1] n) Nat -> Nat
--     build FZ      ks  = 0
--     build (FS FZ) [x] = S x
