module Beer (song) where

import Data.Char (toUpper)
import Data.List (concatMap)

bottle :: Int -> String
bottle num
  | num == 1 = "1 bottle"
  | num == 0 = "no more bottles"
  | otherwise = (show num) ++ " bottles"

firstLine :: Int -> String
firstLine num = bottles ++ " of beer on the wall, " ++ bottles ++ " of beer.\n"
    where bottles = bottle num

secondLine :: Int -> String
secondLine num = "Take " ++ pronoun ++ " down and pass it around, " ++ (bottle $ num - 1) ++ " of beer on the wall.\n\n"
    where pronoun = if num == 1 then "it" else "one"

stanza :: Int -> String
stanza = (++) <$> firstLine <*> secondLine

finalStanza :: String
finalStanza = ((toUpper . head) fLine : tail fLine) ++ "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
    where fLine = firstLine 0

song :: String
song = concatMap stanza [99,98..1] ++ finalStanza