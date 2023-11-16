module BUS.Language
  ( Operator (..),
    Term (..),
    Language (..),
    Env,
    eval,
  )
where

import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter

data Operator val = Operator
  { name :: Text,
    arity :: Int,
    deno :: [val] -> Maybe val
  }

instance Pretty (Operator v) where
  pretty (Operator name arity deno) =
    let fields =
          [ sep ["name", "=", pretty name],
            sep ["arity", "=", pretty arity],
            sep ["deno", "=", "#<" <> pretty name <> ">"]
          ]
     in "Operator" <+> encloseSep lbrace rbrace comma fields

instance Eq (Operator v) where
  (==) = (==) `on` name

instance Ord (Operator v) where
  (<=) = (<=) `on` name

data Term val
  = Symbol Text
  | Constant val
  | Application (Operator val) [Term val]
  deriving (Eq, Ord)

instance Pretty val => Pretty (Term val) where
  pretty (Symbol tx) = "?" <> pretty tx
  pretty (Constant c) = pretty c
  pretty (Application op rands) = parens $ sep $ pretty op : map pretty rands

type Env = Map Text

data Language val = Language
  { constants :: [val],
    operators :: Env (Operator val)
  }

eval :: Env v -> Term v -> Maybe v
eval env (Constant c) = Just c
eval env (Symbol s) = Map.lookup s env
eval env (Application (Operator name arity deno) terms) =
  mapM (eval env) terms >>= deno
