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
record Operator Value where
  constructor MkOperator
  arity : Nat
  name : String
  denotation : Vect arity Value -> Maybe Value

public export
record Language (Value : Type) where
  constructor MkLanguage
  constants : List Value
  rators : SortedMap String (Operator Value)

public export
data Term : Type -> Type where
  Symbol : String -> Term t
  Constant : t -> Term t
  Application : (op : Operator t) -> Vect op.arity (Term t) -> Term t

public export 
Show (Operator v) where
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
eval env (Application (MkOperator name ar denotation) xs) = 
  do subterms <- sequence $ map (\k => eval env (assert_smaller xs k)) xs
     denotation subterms

operatorsOfArity : (l : Language val) -> (n : Nat) -> List (Operator val)
operatorsOfArity l n = search (values l.rators)
  where 
    search : List (Operator val) -> List (Operator val)
    search [] = []
    search (op :: xs) = 
      case decEq op.arity n of
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
