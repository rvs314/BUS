module Tests

import Data.Vect
import Data.SortedMap
import Data.Fuel
import Data.Either
import Data.String
import Bus
import Math
import Expr
import String
import Lists
import Array
import Scheme
import Language
import Folds
import Dan

record Test (Value : Type) where
  constructor MkTest
  name: String
  free: Env Value
  expected: Value

data Fail t = CodegenFailure
            | EvalFailure (Term t)
            | Incorrect (Term t) t

explain : Show t => Fail t -> Test t -> String
explain fail test = unlines $ ["Free Variables:"]
                          <+> map (\(v, k) => "\{v} => \{show k}") 
                              (SortedMap.toList test.free)
                          <+> ["Expected => \{show test.expected}"]
                          <+> case fail of
                                CodegenFailure => 
                                  ["BUS failed to generate code"]
                                (EvalFailure x) => 
                                  [ "Execution failed"
                                  , "Generated Code:"  
                                  , show x]
                                (Incorrect x y) => 
                                  [ "Code did not generate correct result"
                                  , "Generated Code:"
                                  , show x
                                  , "Evaluation Result:"
                                  , show x]

evalTest : (Eq t, Ord t, Show t) => Language t -> Test t -> Either (Fail t) ()
evalTest lang (MkTest nm free expected) = 
         do code <- maybeToEither CodegenFailure
                       $ bus (limit 5) lang free expected
            result <- maybeToEither (EvalFailure code)
                       $ eval free code
            if result == expected
              then Right ()
              else Left (Incorrect code result)

runTest : (Eq t, Ord t, Show t) => Language t -> Test t -> IO ()
runTest l t = putStrLn $ case evalTest l t of
                           (Left fail) => "Test \{t.name} FAILED:\n\{explain fail t}"
                           (Right ()) => "Test \{t.name} passed"

showTest : (Ord t, Show t) => Language t -> Test t -> IO ()
showTest lang (MkTest name free expected) = 
  do let Just code = bus (limit 10) lang free expected
       | Nothing => putStrLn "Did not generate code in time"
     putStr "Generated: "
     printLn code

runTests : (Eq t, Ord t, Show t) => {default runTest testRunner : Language t -> Test t -> IO ()} 
                                    -> Language t -> List (Test t) -> IO ()
runTests {testRunner} l = traverse_ (testRunner l)


test : String -> List (Pair String t) -> t -> Test t
test str xs x = MkTest str (SortedMap.fromList xs) x

mathTests : List (Test (Vect 4 Nat))
mathTests = [ test "Identity" [("x", [0, 1, 2, 3])] [0, 1, 2, 3]
            , test "Add 1"    [("x", [0, 1, 2, 3])] [1, 2, 3, 4]
            , test "Double"   [("x", [0, 1, 2, 3])] [0, 2, 4, 6]
            , test "Const 0"  [("x", [0, 1, 2, 3])] [0, 0, 0, 0]
            , test "Const 4"  [("x", [0, 1, 2, 3])] [4, 4, 4, 4]
            , test "Add"      [("x", [0, 1, 2, 3])
                              ,("y", [9, 2, 4, 3])] [9, 3, 6, 6]
            ]

listTests : List (Test (Vect 4 LObj))
listTests = [ test "Identity" [("x", [Const 1, Nil, Cons Nil (Const 2), Cons Nil Nil])] 
                              [Const 1, Nil, Cons Nil (Const 2), Cons Nil Nil]
            , test "Or"       [("x", [ Cons (Const 1) (Const 2) 
                                     , (Cons Nil (Const 2))
                                     , Cons Nil Nil
                                     , Cons (Const 1) Nil])]
                              [Const 1, Const 2, Nil, Const 1]
            ]

schemeTests : List (Test (Vect 4 SchemeObj))
schemeTests = [ test "Dup" [("x", [Nil, 'B', ConsPair 'B' Nil, ConsPair Nil Car])]
                [ ConsPair Nil Nil
                , ConsPair 'B' 'B'
                , ConsPair (ConsPair 'B' Nil) (ConsPair 'B' Nil)
                , ConsPair (ConsPair Nil Car) (ConsPair Nil Car)]
              ]

recMathTests : List (String, List (Nat, Nat))
recMathTests = [("double" ,
                 [ (0, 0)
                 , (1, 2)
                 , (2, 4)
                 , (3, 8)
                 , (4, 16) ])]

recExprTest : List (Expr, Expr)
recExprTest = [ (Zero, Zero) , (Succ Zero, Succ Zero) , 
                (Succ (Succ Zero), Succ (Succ Zero)) , 
                ((Plus (Succ Zero) (Succ (Succ Zero))), Succ (Succ (Succ Zero))) , 
                ((Plus Zero (Succ (Succ Zero))), Succ (Succ Zero)) , 
                ((Minus (Succ (Succ Zero)) (Succ (Succ Zero))), Zero) , 
                ((Minus (Succ (Succ Zero)) (Succ Zero)), Succ Zero) ]

public export
main : IO ()
main = do
  putStrLn "Math Tests"
  runTests (Array 4 Math) mathTests
  putStrLn "LObj Tests"
  runTests (Array 4 Lists) listTests
  putStrLn "Scheme Tests"
  runTests (Array 4 Scheme) schemeTests
  putStrLn "DAN Tests:"
  for_ recMathTests (\(name, io) => do putStrLn "Test \{name}"
                                       printLn $ dan (limit 5) Math io natView )
  putStrLn "Simple evaluator"
  printLn $ dan (limit 5) ExprLang recExprTest exprView

--   test Math  [1, 2, 3, 4] [("x", [0, 1, 2, 3])]
--   test Math  [2, 4, 6, 8] [("x", [2, 4, 6, 8])]
--   test Math  [3, 6, 9, 12] [("x", [2, 4, 6, 8]), ("y", [3, 2, 3, 4])]
--   test Lists [Const 1, Nil, Nil, Nil] [("x", [Nil, Nil, Const 1, Const 1]),
--                                   ("y", [Nil, Const 1, Nil, Const 1])]
--   test Lists [Const 1, Const 2, Nil, Const 1] 
--         [("x", [(Cons (Const 1) (Const 2)), (Cons Nil (Const 2)), (Cons Nil Nil), (Cons (Const 1) Nil)])]
