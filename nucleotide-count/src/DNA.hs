module DNA (nucleotideCounts) where

import Data.Map (Map, fromList, unionWith)
import Data.List (group, sort)

nucleotides :: String
nucleotides = "ATGC"

isNucleotide :: Char -> Bool
isNucleotide = (flip elem) nucleotides

countBase :: String -> (Char, Int)
countBase xs = (head xs, length xs)

emptyCounts :: Map Char Int
emptyCounts = fromList $ zip nucleotides (repeat 0)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts dna
  | not $ all isNucleotide dna = Left "Invalid DNA sequence."
  | otherwise = Right $ unionWith (+) emptyCounts ((fromList . (map countBase) . group . sort) dna)