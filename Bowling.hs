module Bowling (score) where

score :: [Int] -> Int
score = scoreRec 1

scoreRec :: Int -> [Int] -> Int
scoreRec _ [] = 0
scoreRec _ [_] = div 1 0
scoreRec n [x, y, z]
  | isLastFrame n =
    x + y + z
scoreRec n (x:y:z:rest)
  | isStrike x =
    10 + y + z + scoreRec (succ n) (y:z:rest)
  | isSpare x y =
    10 + z + scoreRec (succ n) (z:rest)
scoreRec n (x:y:rest) =
  x + y + scoreRec (succ n) rest

isStrike :: Int -> Bool
isStrike = (==) 10

isSpare :: Int -> Int -> Bool
isSpare x y = x + y == 10

isLastFrame :: Int -> Bool
isLastFrame = (==) 10
