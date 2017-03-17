module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance strand1 strand2
  | length strand1 /= length strand2 = Nothing
  | otherwise = Just $ foldr f 0 (zip strand1 strand2)
  where f (a, b) dist
          | a == b = dist
          | otherwise = dist + 1
          

