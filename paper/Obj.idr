module Obj

namespace Obj 
  data Obj = Zero
           | Succ Obj
           | Nil
           | Cons Obj Obj
  
  add : Obj -> Obj -> Obj
  add Zero y = y
  add (Succ x) y = add x (Succ y)
  add [] y = Nil
  add (Cons x z) y = Nil
  
  sum : Obj -> Obj
  sum Zero       = Zero
  sum (Succ o)   = Succ (sum o)
  sum Nil        = Zero
  sum (Cons a d) = add (sum a) (sum d)
  
  o : Obj
  o = Cons (Succ Nil) (Succ Nil)
  
  foldObj : (onZero : Obj) ->
            (onSucc : Obj -> Obj) ->
            (onNil : Obj) ->
            (onCons : Obj -> Obj -> Obj) ->
            Obj -> Obj
  foldObj onZero onSucc onNil onCons = rec
    where
      rec : Obj -> Obj
      rec Zero = onZero
      rec (Succ x) = onSucc (rec x)
      rec [] = onNil
      rec (Cons x y) = onCons (rec x) (rec y)

  sum' = foldObj Zero Succ Zero add

namespace Lst
  data List a = Cons a (Lst.List a)
              | Nil
  
  foldList : (onPair : el -> acc -> acc) -> (onNil : acc) -> Lst.List el -> acc
  foldList onPair onNil = rec
    where
      rec : Lst.List el -> acc
      rec Nil = onNil
      rec (Cons x xs) = onPair x (rec xs)
  
    
    
