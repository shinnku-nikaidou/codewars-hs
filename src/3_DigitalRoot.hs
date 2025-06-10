module DigitalRoot where

import Test.Hspec

-- Digital root is the recursive sum of all the digits in a number.

-- Given n, take the sum of the digits of n. If that value has more than one digit, continue reducing in this way until a single-digit number is produced. The input will be a non-negative integer.

-- Examples
--     16  -->  1 + 6 = 7
--    942  -->  9 + 4 + 2 = 15  -->  1 + 5 = 6
-- 132189  -->  1 + 3 + 2 + 1 + 8 + 9 = 24  -->  2 + 4 = 6
-- 493193  -->  4 + 9 + 3 + 1 + 9 + 3 = 29  -->  2 + 9 = 11  -->  1 + 1 = 2

digitalRoot :: (Integral a) => a -> a
digitalRoot n
  | n < 10 = n
  | otherwise = digitalRoot (sum $ digit (show (fromIntegral n)))
  where
    r :: (Integral a) => Char -> a
    r c = fromIntegral (read [c] :: Int)
    digit :: (Integral a) => [Char] -> [a]
    digit = map r

-- ...

spec :: Spec
spec = do
  describe "Testing solution:" $ do
    it "Should compute non-recursive roots, and 0:" $ do
      digitalRoot 16 `shouldBe` 7
      digitalRoot 0 `shouldBe` 0
    it "Should compute recursive roots:" $ do
      digitalRoot 195 `shouldBe` 6
      digitalRoot 992 `shouldBe` 2
      digitalRoot 999999999999 `shouldBe` 9
      digitalRoot 167346 `shouldBe` 9