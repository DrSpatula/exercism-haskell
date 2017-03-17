module School (School, add, empty, grade, sorted) where

import qualified Data.Map as M
import Data.List (sort)

type School = M.Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student school = M.insertWith (++) gradeNum [student] school 

empty :: School
empty = M.empty

grade :: Int -> School -> [String]
grade gradeNum school = 
    case M.lookup gradeNum school of
      Nothing -> []
      (Just xs) -> sort xs

sorted :: School -> [(Int, [String])]
sorted school = (fmap . fmap) sort (M.toList school)
