module Bowling (score) where

score :: [Int] -> Int
score = scoreRec 1

scoreRec :: Int -> [Int] -> Int
scoreRec _ [] = 0
scoreRec _ [_] = div 1 0
scoreRec 10 [x, y, z] = x + y + z
scoreRec n (10:y:z:rest) =  10 + y + z + scoreRec (n + 1) (y:z:rest)
scoreRec n (x:y:z:rest)
  | x + y == 10 =
    10 + z + scoreRec (n + 1) (z:rest)
scoreRec n (x:y:rest) = x + y + scoreRec (n + 1) rest
