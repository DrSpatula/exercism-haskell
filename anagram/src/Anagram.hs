module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (f xs) xss
  where 
    sortLower = sort . (map toLower)
    f w1 w2 = map toLower w1 /= map toLower w2 && sortLower w1 == sortLower w2
