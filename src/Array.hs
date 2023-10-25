module Array (array, prettyPrint) where

import BUS
import Data.List
import qualified Data.Map.Strict as Map
import Language
import Math

array :: Int -> Language v -> Language [v]
array k (Language consts ops) = Language newConsts newOps
  where
    newConsts = replicate k <$> consts
    newOps = (\op -> op {deno = scatter (deno op)}) <$> ops
    scatter fn = traverse fn . transpose

prettyPrint :: Show val => Term [val] -> String
prettyPrint (Symbol s) = s
prettyPrint (Constant c) = show (head c)
prettyPrint (Application (Operator name _ _) ts) =
  "(" ++ unwords (name : map prettyPrint ts) ++ ")"
