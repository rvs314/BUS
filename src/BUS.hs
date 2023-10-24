module BUS () where

import Data.Map (Map)
import qualified Data.Map as Map
import Language
import Numeric.Natural

-- | Given a sum and a length, return all lists of the given length which add up to the sum
scatter :: Natural -> Natural -> [[Natural]]
scatter 0 0 = [[]]
scatter _ 0 = []
scatter sum n =
  do
    (me, you) <- sumsOf (sum, 0)
    rest <- scatter you (n - 1)
    pure $ me : rest
  where
    sumsOf (0, m) = [(0, m)]
    sumsOf (n, m) = (n, m) : sumsOf (n - 1, m + 1)

data Subterm v = Subterm
  { term :: Term v,
    val :: v
  }

data Corpus v = Corpus
  { size :: Natural,
    bySize :: [[Subterm v]],
    byDeno :: Map v (Subterm v)
  }

initialCorpus :: Ord v => Language v -> Env v -> Corpus v
initialCorpus lang env = Corpus 1 [atoms] denos
  where
    consts = map (\c -> Subterm (Constant c) c) $ constants lang
    syms = map (\(s, v) -> Subterm (Symbol s) v) $ Map.assocs env
    atoms = consts ++ syms
    denos = Map.fromList $ map (\c -> (val c, c)) atoms

grow :: Ord v => Language v -> Corpus v -> Corpus v
grow (Language cs ops) (Corpus size bySize byDeno) = error "TODO"
  where
    newGen = generate (Map.elems ops)
    generate operators = error "TODO"
