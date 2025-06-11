module Reverse where

import Data.List.Split (splitOn)

reverseWords :: String -> String
reverseWords s = init $ mconcat (map ((++ " ") . reverse) (splitOn " " s))
