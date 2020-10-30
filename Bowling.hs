module Bowling (score) where

score :: [Int] -> Int
score [] = 0
score [_] = div 1 0
score [x, y, z] = x + y + z
score (x:y:z:rest)
  | x + y == 10 =
    10 + z + score (z:rest)
score (x:y:rest) = x + y + score rest
