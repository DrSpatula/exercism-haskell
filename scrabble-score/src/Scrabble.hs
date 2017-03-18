module Scrabble (scoreLetter, scoreWord) where

import qualified Data.Map as M
import Data.Char (toUpper)

letterValues :: [(String, Int)]
letterValues = [ ("AEIOULNRST", 1)
               , ("DG",         2)
               , ("BCMP",       3)
               , ("FHVWY",      4)
               , ("K",          5)
               , ("JX",         8)
               , ("QZ",        10) ]

expand :: (String, Int) -> [(Char, Int)]
expand (letters, score) = zip letters (repeat score)

scoreLookup :: M.Map Char Int
scoreLookup = M.fromList $ concatMap expand letterValues

scoreLetter :: Char -> Int
scoreLetter c = (maybe 0 id) $ (flip M.lookup) scoreLookup $ toUpper c

scoreWord :: String -> Int
scoreWord = foldr (\a b -> b + (scoreLetter a)) 0
