module BowlingSpec where

import Bowling

import Test.Hspec

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
      describe "in first frame with a bonus of 1" $ do
        it "scores 12" $ do
          let result = score $ [3, 7, 1, 0] ++ (replicate 16 0)
          result `shouldBe` 12

      describe "of other values" $ do
        it "scores 12" $ do
          let result = score $ [4, 6, 1, 0] ++ (replicate 16 0)
          result `shouldBe` 12

      describe "with different bonus" $ do
        it "scores 14" $ do
          let result = score $ [4, 6, 2, 0] ++ (replicate 16 0)
          result `shouldBe` 14

      describe "in second frame with a bonus of 1" $ do
        it "scores 12" $ do
          let result = score $ [0, 0, 5, 5, 1, 0] ++ (replicate 14 0)
          result `shouldBe` 12

      describe "with points after bonus roll" $ do
        it "scores 13" $ do
          let result = score $ [0, 0, 5, 5, 1, 1] ++ (replicate 14 0)
          result `shouldBe` 13

      describe "with spare in last frame" $ do
        it "scores 11" $ do
          let result = score $ (replicate 18 0) ++ [5, 5, 1]
          result `shouldBe` 11
