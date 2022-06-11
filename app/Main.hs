module Main where

import Entity

main :: IO ()
main = do
  let lvs = map (lvToExp' 1000) [1 .. 10]
  let exps = map (expToLv' 1000) lvs
  print lvs
  print exps
  putStrLn "Done"
