module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map, fromList, findWithDefault)
import Data.List (lines, sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

defaultStudents :: [String]
defaultStudents = [ "Alice", "Bob", "Charlie"
                  , "David", "Eve", "Fred"
                  , "Ginny", "Harriet", "Ileana"
                  , "Joseph", "Kincaid", "Larry" ]

toPlant :: Char -> Plant
toPlant 'C' = Clover
toPlant 'G' = Grass
toPlant 'R' = Radishes
toPlant 'V' = Violets

splitGardenString :: String -> [String]
splitGardenString input = reverse $ f (lines input) []
  where 
    f [[],[]] plants = plants
    f [rowA, rowB] plants = f [(drop 2 rowA), (drop 2 rowB)] $ ((take 2 rowA) ++ (take 2 rowB)) : plants

toPlants :: String -> [[Plant]]
toPlants = ((map . map) toPlant) . splitGardenString

defaultGarden :: String -> Map String [Plant]
defaultGarden plants = fromList $ zip defaultStudents (toPlants plants)

garden :: [String] -> String -> Map String [Plant]
garden students plants = fromList $ zip (sort students) (toPlants plants)

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants student garden = findWithDefault [] student garden
