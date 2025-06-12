module LastDigit where

import Test.Hspec
import Test.QuickCheck

lastDigit :: Integer -> Integer -> Integer
lastDigit a b
  | b == 0 = 1
  | a == 0 = 0
  | a == 1 = 1
  | a == 2 = case b `mod` 4 of
      0 -> 6
      1 -> 2
      2 -> 4
      3 -> 8
  | a == 3 = case b `mod` 4 of
      0 -> 1
      1 -> 3
      2 -> 9
      3 -> 7
  | a == 4 = case b `mod` 2 of
      0 -> 6
      1 -> 4
  | a == 6 = 6
  | a == 5 = 5
  | a == 7 = case b `mod` 4 of
      0 -> 1
      1 -> 7
      2 -> 9
      3 -> 3
  | a == 8 = case b `mod` 4 of
      0 -> 6
      1 -> 8
      2 -> 4
      3 -> 2
  | a == 9 = case b `mod` 2 of
      0 -> 1
      1 -> 9
  | a >= 10 = lastDigit (a `mod` 10) b

spec :: Spec
spec = do
  describe "lastDigit" $ do
    it "should work for some examples" $ do
      lastDigit 4 1 `shouldBe` 4
      lastDigit 4 2 `shouldBe` 6
      lastDigit 9 7 `shouldBe` 9
      lastDigit 10 (10 ^ 10) `shouldBe` 0
      lastDigit (2 ^ 200) (2 ^ 300) `shouldBe` 6
    it "should work for x ^ 0" $ property $ \(NonNegative x) ->
      lastDigit x 0 `shouldBe` 1
