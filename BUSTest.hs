{-# LANGUAGE OverloadedStrings  #-}

module Main where

{-
(setq-local haskell-process-args-cabal-repl
  (cons "bus-test" haskell-process-args-cabal-repl))
-}

import BUS
import BUS.Language
import BUS.Language.Array
import BUS.Language.Math
import qualified Data.Map.Strict as Map
import Prettyprinter
import Test.HUnit

ienv :: Env (Many Integer)
ienv = Map.fromList [("x", Many [1, 2, 3])]

budget = 10

synth :: [Integer] -> Maybe (Term (Many Integer))
synth ks = bus (array 3 math) ienv (Many ks) budget

tests :: Test
tests =
  TestList []
    -- [ "Identity function" ~: synth [1, 2, 3] @?= Just "",
    --   "Doubling function" ~: synth [2, 4, 6] @?= Just "(plus x x)",
    --   "Tripling function" ~: synth [3, 6, 9] @?= Just "(plus x (plus x x))",
    --   "Squaring function" ~: synth [1, 4, 9] @?= Just "(times x x)",
    --   "Constant 12"
    --     ~: synth [12, 12, 12]
    --     @?= Just "(times (plus 1 1) (times (plus 1 1) (plus 1 (plus 1 1))))"
    -- ]

main :: IO ()
main = runTestTTAndExit tests
