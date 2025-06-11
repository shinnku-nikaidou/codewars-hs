module TwoSum (twoSum) where

import Data.Foldable (find)
import Data.Maybe (fromJust)
import Test.Hspec

twoSum :: [Int] -> Int -> (Int, Int)
twoSum l s = fromJust (ans >>= \((_, i), (_, j)) -> return (i, j))
  where
    l_index = zip l [0 .. length l - 1]
    ll = [(x, y) | x <- l_index, y <- l_index, snd x < snd y]
    ans = find (\((x, _), (y, _)) -> x + y == s) ll

spec :: Spec
spec = do
  it "finds the matching pair" $ do
    let (i, j) = twoSum [1234, 5678, 9012] 14690
    (min i j, max i j) `shouldBe` (1, 2)
    let (i, j) = twoSum [1, 2, 3] 4
    (min i j, max i j) `shouldBe` (0, 2)
    let (i, j) = twoSum [2, 2, 3] 4
    (min i j, max i j) `shouldBe` (0, 1)