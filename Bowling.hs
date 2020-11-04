module Bowling (score, Roll(..)) where

data Roll = Roll Int
  deriving (Eq)

data Frame = Frame Int
  deriving (Eq)
next :: Frame -> Frame
next (Frame n) = Frame (n + 1)

score :: [Roll] -> Int
score = score' $ Frame 1

score' :: Frame -> [Roll] -> Int
score' f xs = case xs of
  [] -> 0
  r1@(Roll x):r2@(Roll y):r3@(Roll z):rest
    | isLastFrame f ->
      x + y + z
    | isStrike r1 ->
      10 + y + z + score' (next f) (r2:r3:rest)
    | isSpare r1 r2 ->
      10 + z + score' (next f) (r3:rest)
  Roll x:Roll y:rest ->
    x + y + score' (next f) rest
  [_] -> error "Wrong number of rolls"
  where
    isStrike = (==) $ Roll 10
    isSpare (Roll x) (Roll y) = 10 == x + y
    isLastFrame = (==) $ Frame 10
