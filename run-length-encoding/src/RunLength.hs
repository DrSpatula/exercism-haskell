module RunLength (decode, encode) where

import Data.List
import Data.Char

code :: (String -> String) -> String -> String
code f xs 
  | length xs == 1 = xs
  | all isSpace xs = xs
  | otherwise      = f xs

en :: String -> String
en xs = show (length xs) ++ [head xs]

de :: String -> String
de xs = take ((read . init) xs) ((repeat . last) xs) 

decodeGrouper :: String -> [String]
decodeGrouper cs = foldr f [] cs
  where f a bs
          | bs == [] = [[a]]
          | letterSpace || letterLetter || spaceNotSpace = [a] : bs
          | otherwise = (a : (head bs)) : tail bs 
          where 
            lastChar = last (head bs)
            letterSpace = (isLetter a) && (isSpace lastChar)
            letterLetter = (isLetter a) && (isLetter lastChar)
            spaceNotSpace = (isSpace a) && (not $ isSpace lastChar)

decode :: String -> String
decode = concat . (map (code de)) . decodeGrouper

encode :: String -> String
encode = concat . (map (code en)) . group
