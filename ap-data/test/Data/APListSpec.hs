module Data.APListSpec (main, spec) where

import Test.Hspec

import Data.APList

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Show APList" $ do
    it "shows an empty value" $ do
      show (Empty :: APList Int) `shouldBe` "[]"
    it "shows a single value" $ do
      show (APValue 1 Empty) `shouldBe` "[1]"
    it "shows multiple values" $ do
      show (APValue 1 $ APValue 2 $ APValue 3 Empty) `shouldBe` "[1, 2, 3]"
    it "shows string" $ do
      show (APValue "abc" Empty) `shouldBe` "[\"abc\"]"
  describe "Eq APList" $ do
    it "== returns true for lists with the same values" $ do
      (APValue 1 Empty) == (APValue 1 Empty) `shouldBe` True
    it "== returns true for empty lists" $ do
      (Empty :: APList Int) == (Empty :: APList Int) `shouldBe` True
    it "== returns false for lists with different values" $ do
      (APValue 1 Empty) == (APValue 2 Empty) `shouldBe` False
    it "== returns false when comparing APValue to Empty" $ do
      (APValue 1 Empty) == Empty `shouldBe` False
    it "== compares lists of multiple values" $ do
      (APValue 1 $ APValue 2 Empty) == (APValue 1 $ APValue 2 Empty) `shouldBe` True
    it "== compared lists of multiple values which are not equal" $ do
      (APValue 1 $ APValue 2 Empty) == (APValue 2 $ APValue 2 Empty) `shouldBe` False
    it "== compares lists of different lengths" $ do
      (APValue 1 Empty) == (APValue 1 $ APValue 2 Empty) `shouldBe` False
 
    it "/= returns false for lists with the same values" $ do
      (APValue 1 Empty) /= (APValue 1 Empty) `shouldBe` False
    it "/= returns false for empty lists" $ do
      (Empty :: APList Int) /= (Empty :: APList Int) `shouldBe` False
    it "/= returns true for lists with different values" $ do
      (APValue 1 Empty) /= (APValue 2 Empty) `shouldBe` True
    it "/= returns true when comparing APValue to Empty" $ do
      (APValue 1 Empty) /= Empty `shouldBe` True
    it "/= compares lists of multiple values" $ do
      (APValue 1 $ APValue 2 Empty) /= (APValue 1 $ APValue 2 Empty) `shouldBe` False
    it "/= compared lists of multiple values which are not equal" $ do
      (APValue 1 $ APValue 2 Empty) /= (APValue 2 $ APValue 2 Empty) `shouldBe` True
    it "/= compares lists of different lengths" $ do
      (APValue 1 Empty) /= (APValue 1 $ APValue 2 Empty) `shouldBe` True
 