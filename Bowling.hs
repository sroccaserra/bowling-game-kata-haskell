module Bowling (score) where

import Data.List

score :: [Int] -> Int
score (x:y:z:_)
  | x + y == 10 = 10 + 2*z
score xs = sum xs
