module BUS.Language.Math (math) where

import BUS.Language
import qualified Data.Map.Strict as Map

math :: Language Integer
math = Language consts rators
  where
    consts = [0, 1]
    rators = Map.fromList [binop "plus" (+), binop "minus" (-), binop "times" (*)]
    binop name fn = 
      (name, Operator name 2 (\case [x, y] -> Just $ fn x y
                                    _      -> Nothing))
