module SumOfIntervals (sumOfIntervals) where

import Data.List (sortBy)
import Test.Hspec

maxx :: Int
maxx = 100000000000

sumOfIntervals :: [(Int, Int)] -> Int
sumOfIntervals interval = snd $ foldl step ((-maxx, -maxx), 0) $ sorted $ (maxx, maxx) : interval

step :: ((Int, Int), Int) -> (Int, Int) -> ((Int, Int), Int)
step ((l1, r1), ans) (l2, r2)
  | l2 >= r1 = ((l2, r2), ans + (r1 - l1))
  | l2 < r1 = ((l1, max r1 r2), ans)
  | otherwise = error "It can't be"

sorted :: [(Int, Int)] -> [(Int, Int)]
sorted = sortBy (\(x, _) (y, _) -> compare x y)

spec :: Spec
spec = do
  it "Example tests" $ do
    sumOfIntervals [(1, 5)] `shouldBe` 4
    sumOfIntervals [(1, 5), (10, 15), (-1, 3)] `shouldBe` 11
    sumOfIntervals [(1, 5), (6, 10)] `shouldBe` 8
    sumOfIntervals [(1, 5), (1, 5)] `shouldBe` 4
    sumOfIntervals [(1, 4), (7, 10), (3, 5)] `shouldBe` 7

  it "Large intervals" $ do
    sumOfIntervals [(-1000000000, 1000000000)] `shouldBe` 2000000000
    sumOfIntervals [(0, 20), (-100000000, 10), (30, 40)] `shouldBe` 100000030
