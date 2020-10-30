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
        let result = score $ replicate 20 0
        result `shouldBe` 0

    describe "An game with all rolls knocking 1 pin" $ do
      it "scores 20" $ do
        let result = score $ replicate 20 1
        result `shouldBe` 20

    describe "An game with a 5 and all rolls knocking 0 pins" $ do
      it "scores 20" $ do
        let result = score $ 5 : (replicate 19 0)
        result `shouldBe` 5

    describe "A spare" $ do
      describe "in first position with a bonus of 1" $ do
        it "scores 12" $ do
          let result = score $ [3, 7, 1, 0] ++ (replicate 16 0)
          result `shouldBe` 12

      describe "of other values" $ do
        it "scores 12" $ do
          let result = score $ [4, 6, 1, 0] ++ (replicate 16 0)
          result `shouldBe` 12
