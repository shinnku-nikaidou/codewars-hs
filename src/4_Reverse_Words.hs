module Reverse where

import Data.List.Split (splitOn)

reverseWords :: String -> String
reverseWords s = init $ (mconcat . map (++ " ")) $ map reverse $ splitOn " " s
