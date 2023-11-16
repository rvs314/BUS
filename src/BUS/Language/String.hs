module BUS.Language.String where

import BUS.Language
import qualified Data.Map.Strict as Map

data Token = Start | End | Char Char 

data Regex = Number | Lower | Upper | Whitespace | Anything
           | Repeat Regex | Not Regex | Or Regex Regex

data SObj = S String
          | B Bool
          | I Int
          | R Regex
