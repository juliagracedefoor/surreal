module Main where

import           Debug.Trace
import           ISet
import           Surreal
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSize, prop)
import           Test.QuickCheck       hiding ((===))

main :: IO ()
main = hspec $ do
  describe "Surreal" $ do

    describe "isValid" $ do
      it "gives correct results for 0, -1, 1, and &0" $ do
        isValid zero `shouldBe` True
        isValid one `shouldBe` True
        isValid minusone `shouldBe` True
        isValid shadowzero `shouldBe` False
      prop "returns true for arbitrary surreal integers" $
        \x -> isValid (fromInt x)
      prop "returns false for arbitrary surreal numbers where the left and right set have the same integer" $
        \x -> not . isValid $ fromLists [fromInt x] [fromInt x]
      prop "returns false for arbitrary surreal numbers containing &0" $
        \x -> not . isValid $ fromLists [fromInt x] [shadowzero]

    describe "(<==)" $ do
      prop "correctly determines whether one surreal integer is less than or equal to another" $
        \x y -> (fromInt x <== fromInt y) == (x <= y)
      it "correctly compares 0 and &0" $ do
        zero <== shadowzero `shouldBe` False
        shadowzero <== zero `shouldBe` False
        adjacent zero shadowzero `shouldBe` True

    describe "neg" $
      prop "correctly negates arbitrary surreal integers" $
        \x -> neg (fromInt x) === fromInt ((-1) * x)

    describe "add" $ do
      modifyMaxSize (const 11) $ prop "correctly adds together arbitrary surreal integers between -10 and 10" $
        \x y -> add (fromInt x) (fromInt y) === fromInt (x + y)
      modifyMaxSize (const 11) $ prop "correctly subtracts a surreal number by adding its additive inverse" $
        \x y -> add (fromInt x) (neg . fromInt $ y) === fromInt (x - y)
      modifyMaxSize (const 11) $ prop "correctly doubles fractions with a denominator of 2" $
        \x -> let a = fromLists [fromInt x] [fromInt (x+1)]
              in  add a a === fromInt (x * 2 + 1)
      prop "is not slow for values larger than 10" $
        \x y -> add (fromInt x) (fromInt y) === fromInt (x + y)

  describe "ISet" $
    describe "isInt" $ do
      prop "returns true for arbitrary fromInt values" $
        \x -> isInt (fromInt x)
      modifyMaxSize (const 11) $ prop "returns true for arbitrary integers added together" $
        \x y -> isInt $ add (fromInt x) (fromInt y)
      it "works on a surreal integer where both left and right sets are non-empty" $
        let onehalf = fromLists [one] [fromInt 2]
        in isInt $ add onehalf onehalf
