module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits ((.&.))
import Data.Maybe (catMaybes)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show)

instance Enum Allergen where 
  fromEnum Eggs = 1
  fromEnum Peanuts = 2
  fromEnum Shellfish = 4
  fromEnum Strawberries = 8
  fromEnum Tomatoes = 16
  fromEnum Chocolate = 32
  fromEnum Pollen = 64
  fromEnum Cats = 128

  toEnum   1 = Eggs
  toEnum   2 = Peanuts
  toEnum   4 = Shellfish
  toEnum   8 = Strawberries
  toEnum  16 = Tomatoes
  toEnum  32 = Chocolate
  toEnum  64 = Pollen
  toEnum 128 = Cats

allAllergies :: [Allergen]
allAllergies = [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]

hasAllergy :: Int -> Allergen -> Maybe Allergen
hasAllergy score allergen
  | score .&. allergenValue == allergenValue = Just allergen
  | otherwise = Nothing
    where allergenValue = fromEnum allergen

allergies :: Int -> [Allergen]
allergies score = catMaybes $ map (hasAllergy score) allAllergies

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = allergen `elem` (allergies score)
