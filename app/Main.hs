module Main where

import Game (runGame)
import Math (add)

main :: IO ()
main = do
  runGame []
  print (add 3 3)
