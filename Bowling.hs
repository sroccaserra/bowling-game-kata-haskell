module Bowling (score) where

score :: [Int] -> Int
score = scoreRec 1

scoreRec :: Int -> [Int] -> Int
scoreRec n xs = case xs of
  [] -> 0
  [x, y, z]
    | isLastFrame n ->
      x + y + z
  x:y:z:rest
    | isStrike x ->
      10 + y + z + scoreRec (succ n) (y:z:rest)
    | isSpare x y ->
      10 + z + scoreRec (succ n) (z:rest)
  x:y:rest ->
    x + y + scoreRec (succ n) rest
  [_] -> error "Wrong number of rolls"
  where
    isStrike = (==) 10
    isSpare x y = x + y == 10
    isLastFrame = (==) 10
