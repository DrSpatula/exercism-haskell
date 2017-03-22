module Roman (numerals) where

import Data.Char (digitToInt)
import Data.Maybe (catMaybes)

remove :: Int -> Integer -> Integer
remove place x = toInteger $ n - ((n `div` place) * place)
  where n = fromInteger x

removeThousands :: Integer -> Integer
removeThousands = remove 1000

removeHundreds :: Integer -> Integer
removeHundreds = remove 100

removeTens :: Integer -> Integer
removeTens = remove 10


thousands :: Integer -> Maybe String
thousands x
  | x < 1000 = Nothing
  | otherwise = Just $ take ((fromInteger x) `div` 1000) (repeat 'M')


romanPlace :: Int -> Char -> Char -> Char -> (Integer -> Integer) -> Integer -> Maybe String
romanPlace place one five ten removal x
  | n == 0 = Nothing
  | n == 9 = Just [one, ten]
  | n == 4 = Just [one, five]
  | n >= 5 = Just $ five : take (n - 5) (repeat one)
  | otherwise = Just $ take n (repeat one)
    where n = (fromInteger . removal $ x) `div` place

hundreds :: Integer -> Maybe String
hundreds = romanPlace 100 'C' 'D' 'M' removeThousands

tens :: Integer -> Maybe String
tens = romanPlace 10 'X' 'L' 'C' (removeHundreds . removeThousands)

ones :: Integer -> Maybe String
ones = romanPlace 1 'I' 'V' 'X' (removeTens . removeHundreds . removeThousands)


numerals :: Integer -> Maybe String
numerals = do
  th <- thousands
  hn <- hundreds
  tn <- tens
  os <- ones
  return (Just $ concat $ catMaybes [th, hn, tn, os])
