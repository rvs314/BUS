module Bus

import Data.Vect
import Data.Fuel
import Data.List.Elem
import Data.SortedMap
import Language
import Control.Monad.State
import Control.Monad.State.State

%default total

data Sum : Nat -> Type where
  Plus : (n : Nat) -> (m : Nat) -> Sum (n + m)

sumsOf : (n : Nat) -> List (Sum n)
sumsOf k = rewrite sym (plusZeroRightNeutral k) in walk k 0
  where 
    walk : (n : Nat) -> (m : Nat) -> List (Sum (n + m))
    walk 0 m = [Plus 0 m]
    walk (S j) m = 
      let k = Plus (S j) m
          p = rewrite plusSuccRightSucc j m in walk j (S m) 
      in  k :: p

partitions : (sm : Nat) -> (n : Nat) -> List (Vect n Nat)
partitions 0 0 = []
partitions sm 0 = []
partitions sm (S 0) = [[sm]]
partitions sm (S k) = 
  do (Plus head rest) <- sumsOf sm
     ps <- partitions rest k
     pure $ head :: ps

record Subterm (l : Language v) where
  constructor MkSubterm
  term : Term v
  val : v

record Corpus (l : Language v) where
  constructor MkCorpus
  byDeno : SortedMap v (Subterm l)
  bySize : List (List (Subterm l))

initialCorpus : (l : Language v) -> Ord v => Env v -> Corpus l
initialCorpus l nv = 
  let 
    lits : List (Subterm l)
    lits = map (\c => MkSubterm (Constant c) c) l.constants
    syms : List (Subterm l)
    syms = map (\(k, v) => MkSubterm (Symbol k) v) (SortedMap.toList nv)
    consts : List (Subterm l)
    consts = lits ++ syms
    denos : SortedMap v (Subterm l)
    denos = SortedMap.fromList $ map (\c => (val c, c)) consts
  in
    MkCorpus denos [consts]

grow : (l : Language v) -> Ord v => Corpus l -> Corpus l
grow l (MkCorpus byDeno bySize) = 
    let newGen = generate (values l.rators)
        newDenos = SortedMap.fromList $ map (\c => (val c, c)) newGen in
    MkCorpus (mergeLeft byDeno newDenos) (bySize ++ [values newDenos])
  where
    size : Nat
    size = length bySize
    ofSize : Nat -> List (Subterm l)
    ofSize b = join $ toList $ fst <$> indexElem b bySize
    generate : List (Operator v) -> List (Subterm l)
    generate ops = do op@(MkOperator arity _ deno) <- ops
                      budgets <- partitions (pred size) arity
                      subterms <- sequence $ map ofSize budgets
                      value <- toList $ deno $ map val subterms
                      guard $ isNothing $ lookup value byDeno
                      pure $ MkSubterm (Application op (map term subterms)) value

export
bus : Ord v => (f : Fuel) -> (l : Language v) ->
      (nv : Env v) -> (expected : v) -> Maybe (Term v)
bus f l nv ex = term <$> loop f (initialCorpus l nv)
  where
    loop : Fuel -> Corpus l -> Maybe (Subterm l)
    loop Dry c@(MkCorpus byDeno bySize) = lookup ex byDeno 
    loop (More x) c@(MkCorpus byDeno bySize) = lookup ex byDeno <|> loop x (grow l c)

