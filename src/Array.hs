module Array (array) where

import BUS
import Data.List
import qualified Data.Map.Strict as Map
import Language
import Math

array :: Int -> Language v -> Language [v]
array k (Language consts ops) = Language newConsts newOps
  where
    newConsts = replicate k <$> consts
    newOps =
      ( \(Operator name arity deno) ->
          Operator ("scatter (" ++ name ++ ")") arity (scatter deno)
      )
        <$> ops
    scatter fn = traverse fn . transpose
