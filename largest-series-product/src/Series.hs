module Series (largestProduct) where

import Data.Monoid (Product(..))
import Data.Foldable (foldMap)
import Data.Char (digitToInt, isDigit)

prod :: [Int] -> Int
prod = (getProduct . foldMap Product)

spanProduct :: Int -> String -> Int
spanProduct spn digits = prod $ map digitToInt (take spn digits)

largestProduct :: Int -> String -> Maybe Integer
largestProduct size digits  
    | any (not . isDigit) digits = Nothing
    | (length digits) < size = Nothing
    | size < 0 = Nothing
    | all (=='0') digits = Just 0
    | size == 0 = Just 1
    | otherwise = Just $ toInteger $ f (tail digits) (map digitToInt $ take size digits) firstProduct firstProduct
    where 
        firstProduct = spanProduct size digits
        f :: String -> [Int] -> Int -> Int -> Int
        f source dgts prv mx
            | source == [] = mx
            | newProduct > mx = f (tail source) newDigits newProduct newProduct
            | otherwise = f (tail source) newDigits newProduct mx
            where 
                newProduct = 
                    if (head dgts) == 0 then 
                        prod newDigits
                    else
                        (prv `div` (head dgts)) * (digitToInt $ head source)
                    
                newDigits = (tail dgts) ++ [(digitToInt $ head source)]