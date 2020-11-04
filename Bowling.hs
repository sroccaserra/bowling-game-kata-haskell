module Bowling (score, Roll(..)) where

data Roll = Roll Int

data Frame = Frame Int

next :: Frame -> Frame
next (Frame n) = Frame (n + 1)

score :: [Roll] -> Int
score = score' $ Frame 1

score' :: Frame -> [Roll] -> Int
score' f xs = case xs of
  [] -> 0
  Roll x:Roll y:Roll z:rest
    | isLastFrame f ->
      x + y + z
    | isStrike x ->
      10 + y + z + score' (next f) (Roll y:Roll z:rest)
    | isSpare x y ->
      10 + z + score' (next f) (Roll z:rest)
  Roll x: Roll y:rest ->
    x + y + score' (next f) rest
  [_] -> error "Wrong number of rolls"
  where
    isStrike = (==) 10
    isSpare x y = x + y == 10
    isLastFrame (Frame n)= n == 10
