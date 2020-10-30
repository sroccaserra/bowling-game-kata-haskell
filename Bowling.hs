module Bowling (score) where

score :: [Int] -> Int
score [] = 0
score [x] = x
score (x:y:z:rest)
  | x + y == 10 =
    10 + z + score (z:rest)
score (x:y:rest) = x + y + score rest
