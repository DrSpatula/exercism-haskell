module Phone (number) where

import Data.Char (isDigit)
import Control.Monad ((>=>))

otherValidChars :: String
otherValidChars = " -.()"

isValidChar :: Char -> Bool
isValidChar c = isDigit c || elem c otherValidChars

validateChars :: String -> Maybe String
validateChars phone
  | not $ all isValidChar phone = Nothing
  | otherwise = Just $ filter isDigit phone

validateEleven :: String -> Maybe String
validateEleven phone
  | head phone == '1' = Just $ drop 1 phone
  | otherwise = Nothing

validateLength :: String -> Maybe String
validateLength phone
  | len < 10 || len > 11 = Nothing
  | len == 11 = validateEleven phone
  | otherwise = Just phone
  where len = length phone

number :: String -> Maybe String
number = validateChars >=> validateLength
