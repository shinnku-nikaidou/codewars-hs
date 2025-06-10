module Maskify where

maskify :: String -> String
maskify xs
  | length xs <= 4 = xs
  | otherwise = replicate (length xs - 4) '#' ++ reverse (take 4 (reverse xs))
