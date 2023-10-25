module Main where

{-
(setq-local haskell-process-args-cabal-repl
  (cons "bus-test" haskell-process-args-cabal-repl))
-}

import Array
import BUS
import qualified Data.Map.Strict as Map
import Language
import Math
import Test.HUnit

ienv :: Env [Integer]
ienv = Map.fromList [("x", [1, 2, 3])]

budget = 10

synth :: [Integer] -> Maybe String
synth ks = prettyPrint <$> bus (array 3 math) ienv ks budget

tests :: Test
tests =
  TestList
    [ "Identity function" ~: synth [1, 2, 3] @?= Just "x",
      "Doubling function" ~: synth [2, 4, 6] @?= Just "(plus x x)",
      "Tripling function" ~: synth [3, 6, 9] @?= Just "(plus x (plus x x))",
      "Squaring function" ~: synth [1, 4, 9] @?= Just "(times x x)",
      "Constant 12"
        ~: synth [12, 12, 12]
        @?= Just "(times (plus 1 1) (times (plus 1 1) (plus 1 (plus 1 1))))"
    ]

main :: IO ()
main = runTestTTAndExit tests

