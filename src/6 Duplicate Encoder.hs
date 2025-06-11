module Dups where

import Data.Char (toLower)

duplicateEncode :: String -> String
duplicateEncode s = map (\c -> if countChar c ls > 1 then ')' else '(') ls
  where
    ls = map toLower s
    countChar :: Char -> String -> Int
    countChar c = length . filter (== c)
