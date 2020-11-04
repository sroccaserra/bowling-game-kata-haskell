module Bowling (score, Roll(..)) where

data Roll = Roll Int

score :: [Roll] -> Int
score = score' 1

score' :: Int -> [Roll] -> Int
score' n xs = case xs of
  [] -> 0
  Roll x:Roll y:Roll z:rest
    | isLastFrame n ->
      x + y + z
    | isStrike x ->
      10 + y + z + score' n' (Roll y:Roll z:rest)
    | isSpare x y ->
      10 + z + score' n' (Roll z:rest)
  Roll x: Roll y:rest ->
    x + y + score' n' rest
  [_] -> error "Wrong number of rolls"
  where
    n' = succ n
    isStrike = (==) 10
    isSpare x y = x + y == 10
    isLastFrame = (==) 10
