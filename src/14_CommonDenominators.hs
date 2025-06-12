module Codewars.Kata.CommonDenominators where

import Data.List
import Test.Hspec
import Test.QuickCheck

type Ratio a = (a, a) -- Data.Ratio not suitable for this kata

convertFracs :: (Integral a) => [Ratio a] -> [Ratio a]
convertFracs xs = map (\(a, b) -> (a * (d `div` b), d)) xs
  where
    l = map snd xs
    d = foldl1' lcm l

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "convertFracs" $ do
    it "should work for some examples" $ do
      convertFracs [] `shouldBe` []
      convertFracs [(1, 2), (1, 3), (1, 4)] `shouldBe` [(6, 12), (4, 12), (3, 12)]
      convertFracs [(27115, 5262), (87546, 11111111)] `shouldBe` [(301277774765, 58466666082), (460667052, 58466666082)]

    it "should not change lists with a single item" $ do
      conjoin
        [ property $ \(Positive x) -> convertFracs [(x, 1)] `shouldBe` ([(x, 1)] :: [(Int, Int)]),
          property $ \(Positive x) -> convertFracs [(x, 1)] `shouldBe` ([(x, 1)] :: [(Integer, Integer)])
        ]