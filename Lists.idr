module Lists


import Data.Vect  
import Data.SortedMap  
import Language  

%default total

public export
data LObj = Nil | Const Integer | Cons LObj LObj
 
public export 
Eq LObj where
  [] == [] = True
  (Const i) == (Const j) = i == j
  (Cons x z) == (Cons y w) = x == y && w == z
  _ == _ = False

public export
Show LObj where
  show [] = "()"
  show (Const i) = show i
  show (Cons x y) = "(\{show x} \{show y})"

public export
Ord LObj where
  x < Nil = False
  Nil < x = True
  (Const i) < (Const j) = i < j
  (Cons _ _) < (Const j) = False
  (Const _) < (Cons _ _) = True
  (Cons a b) < (Cons c d) = if a == c then b < d else a < c

public export
Lists : Language
Lists = MkLanguage LObj constants (SortedMap.fromList operators)
  where
    constants = the (List LObj) [Nil, Const 1, Const 2, Const 3]
    car : LObj -> Maybe LObj
    car (Cons x y) = Just x
    car _ = Nothing
    cdr : LObj -> Maybe LObj
    cdr (Cons x y) = Just y
    cdr _ = Nothing
    cons : LObj -> LObj -> Maybe LObj
    cons x y = Just (Cons x y)
    annd : LObj -> LObj -> Maybe LObj
    annd Nil _ = Just Nil
    annd _ r = Just r
    oor : LObj -> LObj -> Maybe LObj
    oor Nil k = Just k
    oor k _ = Just k
    noot : LObj -> Maybe LObj
    noot Nil = Just $ Const 1
    noot _   = Just $ Nil
    unop : (LObj -> Maybe LObj) -> String -> (String, (n ** Operator LObj n))
    unop f str = (str, (1 ** MkOperator str (\[k] => f k)))
    binop : (LObj -> LObj -> Maybe LObj) -> String -> (String, (n ** Operator LObj n))
    binop f str = (str, (2 ** MkOperator str (\[k, j] => f k j)))
    operators = the (List _) [ (unop car "car") , (unop cdr "cdr"), (unop noot "not"),
                               (binop cons "cons") , (binop annd "and") , (binop oor "or") ]
