module Grains (square, total) where

import Data.Monoid 

square' :: Integer -> Sum Integer
square' n
  | n == 1 = Sum 1
  | otherwise = (2 * (square' (n - 1)))

square :: Integer -> Maybe Integer
square n
  | n < 1 || n > 64 = Nothing
  | otherwise = Just (getSum (square' n))

total :: Integer
total = (getSum . mconcat . (map square')) [1..64]