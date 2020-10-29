module BowlingSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Bowling

main :: IO ()
main = hspec $ do
  describe "Bowling" $ do
    describe "A gutter game" $ do
      it "scores 0" $ do
        score (replicate 20 0) `shouldBe` 0
