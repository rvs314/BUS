module Main where

import qualified BUS (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
