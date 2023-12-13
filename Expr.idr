module Expr

import Language
import Folds
import Data.Vect
import Data.SortedMap
import Derive.Prelude

%language ElabReflection

public export
data Expr = Zero 
          | Succ Expr 
          | Plus Expr Expr
          | Minus Expr Expr

%runElab derive "Expr" [Eq, Ord, Show]

export
exprView : View Expr
exprView = MkView nk ks break build
  where
    nk : Nat
    nk = 4
  
    ks : Vect 4 Nat
    ks = [0, 1, 2, 2]

    break : Expr -> (n : Fin 4 ** Vect (index n [0, 1, 2, 2]) Expr)
    break Zero        = (FZ ** [])
    break (Succ x)    = (FS FZ ** [x])
    break (Plus x y)  = (FS (FS FZ) ** [x, y])
    break (Minus x y) = (FS (FS (FS FZ)) ** [x, y])

    build : (n : Fin 4) -> Vect (index n [0, 1, 2, 2]) Expr -> Expr
    build FZ xs = Zero
    build (FS FZ) (x :: []) = Succ x
    build (FS (FS FZ)) (x :: (y :: [])) = Plus x y
    build (FS (FS (FS FZ))) (x :: (y :: [])) = Minus x y

export
ExprLang : Language Expr
ExprLang = MkLanguage [Zero] (fromList (map (\k => (name k, k)) [succ, plus, minus]))
  where
    succ : Operator Expr
    succ = MkOperator 1 "succ" (\[e] => Just (Succ e))
  
    plusRec : Expr -> Expr -> Maybe Expr
    plusRec Zero y = Just y
    plusRec (Succ x) y = Succ <$> (x `plusRec` y)
    plusRec _ y = Nothing
  
    minusRec : Expr -> Expr -> Maybe Expr
    minusRec y            Zero = Just y
    minusRec (Succ y) (Succ x) = x `minusRec` y
    minusRec _ _ = Nothing

    plus : Operator Expr
    plus = MkOperator 2 "plus" (\[x, y] => plusRec x y)

    minus : Operator Expr
    minus = MkOperator 2 "minus" (\[x, y] => minusRec x y)
