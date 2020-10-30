module Bowling (score) where

score :: [Int] -> Int
score [] = 0
score [x] = x
score (x:y:z:_)
  | x + y == 10 = 10 + 2*z
score (x:y:rest) = x + y + score rest
