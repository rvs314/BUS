module Language
  ( Operator (..),
    Term (..),
    Language (..),
    Env (..),
    eval
  )
where

import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Operator val = Operator
  { name :: String,
    arity :: Int,
    deno :: [val] -> Maybe val
  }

instance Show (Operator v) where
  show (Operator name arity deno) =
    let fields =
          [ "name = " ++ show name,
            "arity = " ++ show arity,
            "deno = (" ++ name ++ ")"
          ]
     in "Operator {" ++ intercalate ", " fields ++ "}"

instance Eq (Operator v) where
  (==) = (==) `on` name

instance Ord (Operator v) where
  (<=) = (<=) `on` name

data Term val
  = Symbol String
  | Constant val
  | Application (Operator val) [Term val]
  deriving (Eq, Ord, Show)

type Env = Map String

data Language val = Language
  { constants :: [val],
    operators :: Env (Operator val)
  }
  deriving (Show)

eval :: Env v -> Term v -> Maybe v
eval env (Constant c) = Just c
eval env (Symbol s) = Map.lookup s env
eval env (Application (Operator name arity deno) terms) =
  mapM (eval env) terms >>= deno
