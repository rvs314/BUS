module Language

import Data.Vect
import Data.List
import Data.Fuel
import Data.List.Elem
import Data.String
import Sexpr
import Decidable.Equality
import Data.SortedMap

%default total

public export
nary : (n : Nat) -> Type -> Type
nary 0 t = t
nary (S k) t = t -> nary k t

public export
ncurry : {n : Nat} -> nary n a -> Vect n a -> a
ncurry       x [] = x
ncurry       x (y :: xs) = ncurry (x y) xs

public export
record Operator Value (arity : Nat) where
  constructor MkOperator
  name : String
  denotation : Vect arity Value -> Maybe Value

public export
record Language where
  constructor MkLanguage
  Val : Type
  constants : List Val
  rators : SortedMap String (n ** (Operator Val n))

public export
data Term : Type -> Type where
  Symbol : String -> Term t
  Constant : t -> Term t
  Application : {n : Nat} -> Operator t n -> Vect n (Term t) -> Term t

public export 
Show (Operator v n) where
  show = name

public export
Show v => Show (Term v) where
  show (Application x xs) = 
    let seq = toList $ x.name :: (map (\s => show (assert_smaller xs s)) xs) in 
    "(\{unwords seq})"
  show (Constant x) = show x
  show (Symbol str) = str

public export
Env : Type -> Type
Env val = SortedMap String val

export
eval : Env v -> Term v -> Maybe v
eval env (Symbol str) = lookup str env
eval env (Constant x) = Just x
eval env (Application (MkOperator name denotation) xs) = 
  do subterms <- sequence $ map (\k => eval env (assert_smaller xs k)) xs
     denotation subterms

operatorsOfArity : (l : Language) -> (n : Nat) -> List (Operator l.Val n)
operatorsOfArity l n = search (values l.rators)
  where 
    search : List (ar ** Operator l.Val ar) -> List (Operator l.Val n)
    search [] = []
    search (((ar ** op)) :: xs) = 
      case decEq ar n of
        (Yes Refl) => op :: search xs
        (No contra) => search xs

-- public export
-- construct : (l : Language) -> Sexpr -> Maybe (Term l.Val)
-- construct l (Atom str) = 
--   do (k, ks) <- strUncons str
--      if k == '?' 
--      then Just (Symbol ks)
--      else 
    
--   case lookup str l.rators of
--     (Just ((0 ** op))) => Just $ MkTerm 0 op []
--     _ => Nothing
-- construct l (App ((Atom str) :: xs)) = 
--   do let vc = Vect.fromList xs
--      (ar ** op) <- lookup str l.rators
--      case decEq (length xs) ar of
--        (Yes Refl) => do subterms <- 
--                             sequence $ map (\k => 
--                                              construct l (assert_smaller xs k))
--                                            vc
--                         pure $ MkTerm ar op subterms
--        (No contra) => Nothing
-- construct l _ = Nothing
