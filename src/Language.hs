module Language
  ( Operator (..),
    Term (..),
    Language (..),
    Env (..),
    Name (..),
    eval,
  )
where

import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Numeric.Natural

newtype Name = Name {unName :: String}
  deriving (Eq, Ord)

instance Show Name where
  show = unName

data Operator val = Operator
  { name :: Name,
    arity :: Natural,
    deno :: [val] -> Maybe val
  }

instance Eq (Operator v) where
  (==) = (==) `on` name

instance Ord (Operator v) where
  (<=) = (<=) `on` name

data Term val
  = Symbol Name
  | Constant val
  | Application (Operator val) [Term val]
  deriving (Eq, Ord)

instance Show (Operator v) where
  show = show . name

instance Show v => Show (Term v) where
  show (Symbol (Name s)) = "?" ++ s
  show (Constant c) = show c
  show (Application op terms) = "(" ++ unwords (show op : map show terms) ++ ")"

type Env = Map Name

data Language val = Language
  { constants :: [val],
    operators :: Env (Operator val)
  }

eval :: Env v -> Term v -> Maybe v
eval env (Constant c) = Just c
eval env (Symbol s) = Map.lookup s env
eval env (Application (Operator name arity deno) terms) =
  mapM (eval env) terms >>= deno
