module Rot13 where

import Test.Hspec
import Test.QuickCheck
import Text.Printf (printf)

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = filter f [a .. b]
  where
    f :: Int -> Bool
    f x = x == t3 x
    t1 x = map (\c -> read [c]) (show x)
    t2 :: Int -> [(Int, Int)]
    t2 x = zip (t1 x) [1 ..]
    t3 :: Int -> Int
    t3 x = sum $ map (uncurry (^)) (t2 x)

dotest :: Int -> Int -> [Int] -> Spec
dotest a b s =
  it (printf "should return sumDigPow for a, b: %d %d result --> %s \n" a b (show s)) $
    sumDigPow a b `shouldBe` s

spec :: Spec
spec = do
  describe "sumDigNthTerm" $ do
    dotest 1 10 [1, 2, 3, 4, 5, 6, 7, 8, 9]
    dotest 1 100 [1, 2, 3, 4, 5, 6, 7, 8, 9, 89]
    dotest 10 100 [89]
    dotest 90 100 []
    dotest 90 150 [135]
    dotest 50 150 [89, 135]
    dotest 10 150 [89, 135]