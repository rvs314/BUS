
import Data.Vect
import Data.SortedMap
import Data.Fuel
import Bus
import Math
import String
import Lists
import Array
import Language

%default total

testI : Vect 4 Integer -> List (String, Vect 4 Integer) -> IO ()
testI xs ys = 
 do printLn "Free Variables:"
    traverse_ (\(s, v) => printLn "\{s} => \{show v}") ys
    printLn "Expected Values:"
    printLn $ show xs
    let res = bus (limit 10) (Array 4 Math) (SortedMap.fromList ys) xs
    printLn $ "Generated Code: " ++ show res


testL : Vect 4 LObj -> List (String, Vect 4 LObj) -> IO ()
testL xs ys = 
 do printLn "Free Variables:"
    traverse_ (\(s, v) => printLn "\{s} => \{show v}") ys
    printLn "Expected Values:"
    printLn $ show xs
    let res = bus (limit 10) (Array 4 Lists) (SortedMap.fromList ys) xs
    printLn $ "Generated Code: " ++ show res

main : IO ()
main = do
  testI [1, 2, 3, 4] [("x", [0, 1, 2, 3])]
  testI [2, 4, 6, 8] [("x", [2, 4, 6, 8])]
  testI [3, 6, 9, 12] [("x", [2, 4, 6, 8]), ("y", [3, 2, 3, 4])]
  testL [Const 1, Nil, Nil, Nil] [("x", [Nil, Nil, Const 1, Const 1]),
                                  ("y", [Nil, Const 1, Nil, Const 1])]
  testL [Const 1, Const 2, Nil, Const 1] 
        [("x", [(Cons (Const 1) (Const 2)), (Cons Nil (Const 2)), (Cons Nil Nil), (Cons (Const 1) Nil)])]

-- show $ bus (limit 10) (Array 4 Lists) (SortedMap.fromList [("x", [Nil, Nil, Const 1, Const 1]), ("y", [Nil, Const 1, Nil, Const 1])])
--                                          [Const 1, Nil, Nil, Nil]
