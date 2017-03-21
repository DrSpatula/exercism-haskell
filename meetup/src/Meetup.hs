module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, toGregorian, fromGregorian, isLeapYear)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Calendar.MonthDay (monthLength)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Ord, Show)

instance Enum Weekday where 
  toEnum 1 = Monday
  toEnum 2 = Tuesday
  toEnum 3 = Wednesday
  toEnum 4 = Thursday
  toEnum 5 = Friday
  toEnum 6 = Saturday
  toEnum 7 = Sunday

  fromEnum Monday = 1
  fromEnum Tuesday = 2
  fromEnum Wednesday = 3
  fromEnum Thursday = 4
  fromEnum Friday = 5
  fromEnum Saturday = 6
  fromEnum Sunday = 7


data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

getWeekday :: Day -> Weekday
getWeekday d = toEnum wd
  where (_,_,wd) = toWeekDate d 

isWeekday :: Weekday -> Day -> Bool
isWeekday wd d = wd == getWeekday d 

lastDay :: Integer -> Int -> Int
lastDay year = monthLength (isLeapYear year) 

days :: Integer -> Int -> [Int] -> [Day]
days year month = map (fromGregorian year month)

monthDays :: Integer -> Int -> [Day]
monthDays year month = days year month [1..(lastDay year month)]

monthWeekdays :: Integer -> Int -> Weekday -> [Day]
monthWeekdays year month weekday = filter (isWeekday weekday) (monthDays year month)

teenthDays :: Integer -> Int -> [Day]
teenthDays year month = days year month [13..19]

teenth :: Integer -> Int -> Weekday -> Day
teenth year month weekday = head $ filter (isWeekday weekday) (teenthDays year month)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay Teenth weekday year month = teenth year month weekday
meetupDay First weekday year month = head (monthWeekdays year month weekday)
meetupDay Second weekday year month = head $ tail (monthWeekdays year month weekday)
meetupDay Third weekday year month = (monthWeekdays year month weekday) !! 2
meetupDay Fourth weekday year month = (monthWeekdays year month weekday) !! 3
meetupDay Last weekday year month = last (monthWeekdays year month weekday)
