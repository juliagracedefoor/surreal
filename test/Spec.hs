module Main where

import           Surreal
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Surreal" $ do

    describe "isValid" $ do
      it "gives correct results for 0, -1, 1, and &0" $ do
        isValid zero `shouldBe` True
        isValid one `shouldBe` True
        isValid minusone `shouldBe` True
        isValid shadowzero `shouldBe` False
      it "returns true for arbitrary surreal integers" $
        property $ \x -> isValid (fromInt x) == True
      it "returns false for arbitrary surreal numbers where the left and right set have the same integer" $
        property $ \x -> isValid (fromLists [fromInt x] [fromInt x]) == False
      it "returns false for arbitrary surreal numbers containing &0" $
        property $ \x -> isValid (fromLists [fromInt x] [shadowzero]) == False

    describe "(<==)" $ do
      it "correctly determines whether one surreal integer is less than or equal to another" $
        property $ \x y -> ((fromInt x) <== (fromInt y)) == (x <= y)
      it "correctly compares 0 and &0" $ do
        zero <== shadowzero `shouldBe` False
        shadowzero <== zero `shouldBe` False

    describe "neg" $ do
      it "correctly negates arbitrary surreal integers" $
        property $ \x -> neg (fromInt x) == fromInt ((-1) * x)
