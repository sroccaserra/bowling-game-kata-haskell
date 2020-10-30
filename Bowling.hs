module Bowling (score) where

score :: [Int] -> Int
score = score' 1

score' :: Int -> [Int] -> Int
score' n xs = case xs of
  [] -> 0
  [x, y, z]
    | isLastFrame n ->
      x + y + z
  x:y:z:rest
    | isStrike x ->
      10 + y + z + score' (succ n) (y:z:rest)
    | isSpare x y ->
      10 + z + score' (succ n) (z:rest)
  x:y:rest ->
    x + y + score' (succ n) rest
  [_] -> error "Wrong number of rolls"
  where
    isStrike = (==) 10
    isSpare x y = x + y == 10
    isLastFrame = (==) 10
