module SumOfMultiples (sumOfMultiples) where

import Data.Set (toList, fromList)

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples factors limit = (sum . toList . fromList) [x | x <- [1..(limit - 1)], y <- factors, x `mod` y == 0]
