module LeapYear (isLeapYear) where

isDivisibleBy4 :: Integer -> Bool
isDivisibleBy4 year = (year `mod` 4) == 0

isDivisibleBy100 :: Integer -> Bool
isDivisibleBy100 year = (year `mod` 100) == 0

isDivisibleBy400 :: Integer -> Bool
isDivisibleBy400 year = (year `mod` 400) == 0


isLeapYear :: Integer -> Bool
isLeapYear year
  | (isDivisibleBy400 year) && (isDivisibleBy100 year) = True
  | (isDivisibleBy4 year) && (not $ isDivisibleBy100 year) = True
  |  otherwise = False
