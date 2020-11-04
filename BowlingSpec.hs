module BowlingSpec where

import Bowling

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Bowling" $ do

    describe "A gutter game" $ do
      it "scores 0" $ do
        let result = score $ replicate 20 $ Roll 0
        result `shouldBe` 0

    describe "An game with all rolls knocking 1 pin" $ do
      it "scores 20" $ do
        let result = score $ replicate 20 $ Roll 1
        result `shouldBe` 20

    describe "An game with a 5 and all rolls knocking 0 pins" $ do
      it "scores 20" $ do
        let result = score $ Roll 5 : (replicate 19 $ Roll 0)
        result `shouldBe` 5

    describe "A spare" $ do
      describe "in first frame with a bonus of 1" $ do
        it "scores 12" $ do
          let result = score $ [Roll 3, Roll 7, Roll 1, Roll 0] ++ (replicate 16 $ Roll 0)
          result `shouldBe` 12

      describe "of other values" $ do
        it "scores 12" $ do
          let result = score $ [Roll 4, Roll 6, Roll 1, Roll 0] ++ (replicate 16 $ Roll 0)
          result `shouldBe` 12

      describe "with different bonus" $ do
        it "scores 14" $ do
          let result = score $ [Roll 4, Roll 6, Roll 2, Roll 0] ++ (replicate 16 $ Roll 0)
          result `shouldBe` 14

      describe "in second frame with a bonus of 1" $ do
        it "scores 12" $ do
          let firstThreeFrames = [Roll 0, Roll 0, Roll 5, Roll 5, Roll 1, Roll 0]
          let result = score $ firstThreeFrames ++ (replicate 14 $ Roll 0)
          result `shouldBe` 12

      describe "with points after bonus roll" $ do
        it "scores 13" $ do
          let firstThreeFrames = [Roll 0, Roll 0, Roll 5, Roll 5, Roll 1, Roll 1]
          let result = score $ firstThreeFrames ++ (replicate 14 $ Roll 0)
          result `shouldBe` 13

      describe "with spare in last frame" $ do
        it "scores 11" $ do
          let result = score $ (replicate 18 $ Roll 0) ++ [Roll 5, Roll 5, Roll 1]
          result `shouldBe` 11

    describe "A strike" $ do
      describe "in first frame" $ do
        it "scores 14" $ do
          let result = score $ [Roll 10, Roll 1, Roll 1] ++ (replicate 16 $ Roll 0)
          result `shouldBe` 14

      describe "in ninth frame with a bonus of 2" $ do
        it "scores 14" $ do
          let result = score $ (replicate 16 $ Roll 0) ++ [Roll 10, Roll 1, Roll 1]
          result `shouldBe` 14

      describe "in last frame" $ do
        it "scores 14" $ do
          let result = score $ (replicate 18 $ Roll 0) ++ [Roll 10, Roll 1, Roll 1]
          result `shouldBe` 12

    describe "A perfect game" $ do
      it "scores 300 points" $ do
        let result = score $ replicate 12 $ Roll 10
        result `shouldBe` 300
