module BUS (bus) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import BUS.Language
import Control.Applicative

sums :: Int -> Int -> [[Int]]
sums 0 0 = [[]]
sums _ 0 = []
sums sum n = do
  (me, you) <- sumsOf (sum, 0)
  rest <- sums you (n - 1)
  pure $ me : rest
  where
    sumsOf (0, m) = [(0, m)]
    sumsOf (n, m) = (n, m) : sumsOf (n - 1, m + 1)

data Subterm v = Subterm
  { term :: Term v,
    val :: v
  }

apply :: Operator v -> [Subterm v] -> Maybe (Subterm v)
apply op@(Operator name arity deno) subterms = do
  guard $ arity == length subterms
  v <- deno $ val <$> subterms
  let t = Application op (term <$> subterms)
  pure $ Subterm t v

mapOfSubterms :: Ord v => [Subterm v] -> Map v (Term v)
mapOfSubterms = Map.fromList . map (\c -> (val c, term c))

subtermsOfMap :: Map v (Term v) -> [Subterm v]
subtermsOfMap = map (uncurry $ flip Subterm) . Map.assocs

data Corpus v = Corpus
  { size :: Int,
    bySize :: [[Subterm v]],
    byDeno :: Map v (Term v)
  }

initialCorpus :: Ord v => Language v -> Env v -> Corpus v
initialCorpus lang env = Corpus 0 [subtermsOfMap denos] denos
  where
    consts = (\c -> Subterm (Constant c) c) <$> constants lang
    syms = (\(n, v) -> Subterm (Symbol n) v) <$> Map.assocs env
    atoms = consts ++ syms
    denos = mapOfSubterms atoms

grow :: Ord v => Language v -> Corpus v -> Corpus v
grow (Language cs ops) (Corpus size bySize byDeno) =
  Corpus (size + 1) (bySize ++ [nextBySize]) nextByDeno
  where
    nextGenList = do
      op@(Operator name arity deno) <- Map.elems ops
      budgets <- sums size arity
      subterms <- traverse (bySize !!) budgets
      maybeToList $ apply op subterms
    nextGen = mapOfSubterms nextGenList
    nextByDeno = byDeno `Map.union` nextGen
    nextBySize = subtermsOfMap $ nextGen `Map.difference` byDeno

bus :: Ord v => Language v -> Env v -> v -> Int -> Maybe (Term v)
bus lang env val fuel = loop (initialCorpus lang env) fuel
  where
    loop corpus 0 = Nothing
    loop corpus fuel =
      Map.lookup val (byDeno corpus) <|> loop (grow lang corpus) (fuel - 1)
