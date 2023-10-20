
module String

import Data.Vect  
import Data.String  
import Data.SortedMap  
import Language  
import Data.List.Elem

partial
public export
Strings : Language
Strings = MkLanguage String constants (SortedMap.fromList operators)
  where
    constants = the (List String) ["a", "b", "c", "1"]
    binop : String -> (String -> String -> String) -> (String, (DPair Nat (Operator String)))
    binop nm fn = (nm, (2 ** MkOperator nm $ Just . (ncurry fn)))
    idx : String -> String -> Maybe String
    idx str ix = do let k = cast $ strLength ix
                    j <- map fst (indexElem (fromInteger (mod k (cast (strLength str)))) (unpack str))
                    pure $ pack [j]
    operators = the (List _) [ binop "concat" (++) , ("index", (2 ** MkOperator "index" (\[a, b] => idx a b))) ] 
