{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module BUS.Language.Array (array, Many(..)) where

import BUS
import Data.List
import Data.Maybe
import Data.List.NonEmpty (NonEmpty, NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import BUS.Language
import Prettyprinter

--- A newtype used for example sets
newtype Many a = Many { unMany :: [a] }
  deriving (Eq, Ord, Show)

array :: Int -> Language v -> Language (Many v)
array k _ | k <= 0 = error "Array must have a positive length"
array k (Language consts ops) = Language newConsts newOps
  where
    newConsts = Many . replicate k <$> consts
    newOps = (\op -> op {deno = scatter (deno op)}) <$> ops
    scatter :: ([a] -> Maybe a) -> ([Many a] -> Maybe (Many a))
    scatter fn listOfMany = Many <$> traverse fn (transpose $ map unMany listOfMany)

instance {-# OVERLAPPING #-} Pretty v => Pretty (Term (Many v)) where
  pretty = pretty . exemplar
    where
      exemplar :: Pretty v => Term (Many v) -> Term v
      exemplar (Constant (Many ks)) = Constant (head ks)
      exemplar (Symbol s) = Symbol s


-- prettyPrint :: Show val => Term [val] -> String
-- prettyPrint (Symbol s) = s
-- prettyPrint (Constant c) = show (head c)
-- prettyPrint (Application (Operator name _ _) ts) =
  -- "(" ++ unwords (name : map prettyPrint ts) ++ ")"
