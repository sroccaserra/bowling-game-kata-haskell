module Bowling (score) where

score :: [Int] -> Int
score = score' 1

score' :: Int -> [Int] -> Int
score' n xs = case xs of
  [] -> 0
  x:y:z:rest
    | isLastFrame n ->
      x + y + z
    | isStrike x ->
      10 + y + z + score' next_n (y:z:rest)
    | isSpare x y ->
      10 + z + score' next_n (z:rest)
  x:y:rest ->
    x + y + score' next_n rest
  [_] -> error "Wrong number of rolls"
  where
    next_n = succ n
    isStrike = (==) 10
    isSpare x y = x + y == 10
    isLastFrame = (==) 10
