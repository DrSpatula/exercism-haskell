module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
  | noSpaceXs == [] = "Fine. Be that way!"
  | xs == upperXs && any (== UppercaseLetter) (map generalCategory upperXs) = "Whoa, chill out!"
  | last noSpaceXs == '?' = "Sure."
  | otherwise = "Whatever."
  where 
    noSpaceXs = concat (words xs)
    upperXs = map toUpper xs
