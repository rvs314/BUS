
module String

import Data.Vect  
import Data.String  
import Data.SortedMap  
import Language  

partial
public export
Strings : Language
Strings = MkLanguage String constants (SortedMap.fromList operators)
  where
    constants = the (List String) [" ", ".", "-", "*", "0", "1", "2", "-1"]
    binop : String -> (String -> String -> String) -> (String, (DPair Nat (Operator String)))
    binop nm fn = (nm, (2 ** MkOperator nm $ Just . (ncurry fn)))
    idx : String -> String -> Maybe String
    idx str ix = 
      do i <- parseInteger ix
         pure $ pack [strIndex str $ i `mod` (strLength str)]
    operators = the (List _) [ binop "concat" (++)
                             , ("index", (2 ** MkOperator "index" (\[a, b] => idx a b))) ] 
