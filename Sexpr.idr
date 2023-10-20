module Sexpr

import Data.String
-- import Data.String.Parser
  
%default total 

public export
data Sexpr = App (List Sexpr) | Atom String

isSpecial : Char -> Bool
isSpecial c = c == '(' || c == ')' || isSpace c

parseAtom : String -> Maybe (String, String)
parseAtom s with (strM s)
  parseAtom "" | StrNil = Just ("", "")
  parseAtom s@(strCons c str) | (StrCons c str) = 
    if isSpecial c
    then Just ("", s)
    else 
      let rst = parseAtom (assert_smaller s str) in
      map (mapFst (strCons c)) rst

mutual
  parseList : String -> Maybe (List Sexpr, String)
  parseList i = (do (p, rs) <- parseSexpr i
                    (ps, rs) <- parseList (assert_smaller i rs)
                    pure (p :: ps, rs))
                <|> Just ([], i)
  
  parseSexpr : String -> Maybe (Sexpr, String)
  parseSexpr s =
    case strUncons s of
      Nothing => Nothing
      Just ('(', rst) =>
        do (res, rst) <- parseList (assert_smaller s rst)
           case strUncons rst of
             Just (')', rst) => pure $ (App res, rst)
             _               => Nothing
      Just (')', rst) => Nothing
      Just (c, str) =>
        if isSpace c
        then parseSexpr (assert_smaller s str)
        else map (mapFst Atom) (parseAtom s)
