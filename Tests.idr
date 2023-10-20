
import Data.Vect
import Data.SortedMap
import Data.Fuel
import Bus
import Math
import String
import Array
import Language

testI : Vect 4 Integer -> List (String, Vect 4 Integer) -> IO ()
testI xs ys = 
 do printLn "Free Variables:"
    traverse_ (\(s, v) => printLn "\{s} => \{show v}") ys
    printLn "Expected Values:"
    printLn $ show xs
    let res = bus (limit 10) (Array 4 Math) (SortedMap.fromList ys) xs
    printLn $ "Generated Code: " ++ show res

partial
testS : Vect 4 String -> List (String, Vect 4 String) -> IO ()
testS xs ys = 
 do printLn "Free Variables:"
    traverse_ (\(s, v) => printLn "\{s} => \{show v}") ys
    printLn "Expected Values:"
    printLn $ show xs
    let res = bus (limit 10) (Array 4 Strings) (SortedMap.fromList ys) xs
    printLn $ "Generated Code: " ++ show res

partial
main : IO ()
main = do
  testI [1, 2, 3, 4] [("x", [0, 1, 2, 3])]
  testI [2, 4, 6, 8] [("x", [2, 4, 6, 8])]
  testI [3, 6, 9, 12] [("x", [2, 4, 6, 8]), ("y", [3, 2, 3, 4])]
  testS ["a", "b", "c", "d"] [("x", ["aa", "ab", "ac", "ad"])]
