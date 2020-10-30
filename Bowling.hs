module Bowling (score) where

score :: [Int] -> Int
score = scoreRec 1

scoreRec :: Int -> [Int] -> Int
scoreRec _ [] = 0
scoreRec _ [_] = div 1 0
scoreRec 10 [x, y, z] = x + y + z
scoreRec n (x:y:z:rest)
  | isStrike x =
    10 + y + z + scoreRec (n + 1) (y:z:rest)
  | isSpare x y =
    10 + z + scoreRec (n + 1) (z:rest)
scoreRec n (x:y:rest) = x + y + scoreRec (n + 1) rest

isStrike :: Int -> Bool
isStrike = (==) 10

isSpare :: Int -> Int -> Bool
isSpare x y = x + y == 10
