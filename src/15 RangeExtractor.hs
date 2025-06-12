module RangeExtractor.JorgeVS.Kata where

import Data.List
import Test.Hspec

solution :: [Integer] -> String
solution = intercalate "," . map formatGroup . consecutiveGroups
  where
    consecutiveGroups :: [Integer] -> [[Integer]]
    consecutiveGroups [] = []
    consecutiveGroups (x : xs) = go [x] xs
      where
        go acc [] = [acc]
        go acc (y : ys)
          | y == last acc + 1 = go (acc ++ [y]) ys
          | otherwise = acc : go [y] ys
    formatGroup :: [Integer] -> String
    formatGroup g =
      case length g of
        1 -> show a
        2 -> show a ++ "," ++ show b
        _ -> show a ++ "-" ++ show b
      where
        a = head g
        b = last g
