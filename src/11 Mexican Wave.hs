module MexicanWave where

import Data.Char (toUpper)
import Data.List

wave :: String -> [String]
wave s =
  map (\(i, v) -> zipWith (\j y -> (if i == j then toUpper y else y)) [0 ..] v) $
    filter (\(i, v) -> v !! i /= ' ') $
      zip [0 ..] $
        replicate (length s) s
