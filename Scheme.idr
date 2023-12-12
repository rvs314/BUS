module Scheme

import Data.Vect
import Data.String
import Data.SortedMap
import Derive.Prelude
import Language

%language ElabReflection
%default total

public export
data SchemeObj = Nil
               | Symbol String
               | ConsPair SchemeObj SchemeObj
               | SymbolP | NilP | PairP
               | Car | Cdr | Cons
               | PartialCons SchemeObj

%runElab derive "SchemeObj" [Eq, Ord, Show]

export
FromString SchemeObj where
  fromString = Symbol

export
FromChar SchemeObj where
  fromChar = Symbol . String.singleton

export
Scheme : Language SchemeObj
Scheme = MkLanguage 
         ([Nil, Car, Cdr, Cons] <+> letters)
         $ SortedMap.fromList [("apply", 
                                MkOperator 2 "apply" 
                                $ \[op, arg] => !(denotation op) arg)]
  where
    symbolp : SchemeObj -> SchemeObj
    symbolp (Symbol str) = 'T'
    symbolp _ = Nil

    nilp    : SchemeObj -> SchemeObj
    nilp [] = 'T'
    nilp _ = Nil
    
    pairp    : SchemeObj -> SchemeObj
    pairp (ConsPair _ _) = 'T'
    pairp _ = Nil
  
    car : SchemeObj -> Maybe SchemeObj
    car (ConsPair a _) = Just a
    car _ = Nothing
  
    cdr : SchemeObj -> Maybe SchemeObj
    cdr (ConsPair _ d) = Just d
    cdr _ = Nothing
  
    denotation : SchemeObj -> Maybe (SchemeObj -> Maybe SchemeObj)
    denotation SymbolP = Just (Just . symbolp)
    denotation NilP = Just (Just . nilp)
    denotation PairP = Just (Just . pairp)
    denotation Car = Just car
    denotation Cdr = Just cdr
    denotation Cons = Just (Just . PartialCons)
    denotation (PartialCons a) = Just (Just . ConsPair a)
    denotation _ = Nothing

    letters : List SchemeObj
    letters = fromChar <$> unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

