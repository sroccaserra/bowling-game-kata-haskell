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

    describe "An game with all rolls knocking 1 pin" $ do
      it "scores 20" $ do
        score (replicate 20 1) `shouldBe` 20

    describe "An game with a 5 and all rolls knocking 0 pins" $ do
      it "scores 20" $ do
        score (5:(replicate 19 0)) `shouldBe` 5
