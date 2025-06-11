module MexicanWave where

import Data.Char (toUpper)
import Data.List
import Control.Monad
import Data.Bool
import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import Data.Bifunctor

wave' :: String -> [String]
wave' s =
  map (\(i, v) -> zipWith (\j y -> (if i == j then toUpper y else y)) [0 ..] v) $
    filter (\(i, v) -> v !! i /= ' ') $
      zip [0 ..] $
        replicate (length s) s


wave :: String -> [String]
wave =
  catMaybes . (zipWith go <$> inits <*> tails)
  where
    go as ass = do
      (b, bs) <- uncons ass
      guard (not $ isSpace b)
      return (as ++ (toUpper b : bs))
