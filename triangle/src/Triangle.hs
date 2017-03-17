module Triangle (TriangleType(..), triangleType) where

import Data.Monoid (All(..))

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

inequalityCheck :: (Num a, Eq a, Ord a) => (a,a) -> a -> Bool
inequalityCheck (x,y) z = (x + y) >= z

pairs :: a -> a -> a -> [(a,a)]
pairs x y z = [(x,y), (y,z), (z,x)]

allTrue :: [Bool] -> Bool
allTrue = getAll . foldMap All

passesInequality :: (Num a, Eq a, Ord a) => a -> a -> a -> Bool
passesInequality x y z = allTrue (zipWith inequalityCheck (pairs x y z) [z,x,y])

isIllegal :: (Num a, Eq a, Ord a) => a -> a -> a -> Bool
isIllegal x y z = (not $ passesInequality x y z) || (any ((==) 0) [x,y,z])

isEquilateral :: (Num a, Eq a, Ord a) => a -> a -> a -> Bool
isEquilateral x y z = allTrue (map (uncurry (==)) (pairs x y z)) 

isScalene :: (Num a, Eq a, Ord a) => a -> a -> a -> Bool
isScalene x y z = allTrue (map (uncurry (/=)) (pairs x y z))

isIsosceles :: (Num a, Eq a, Ord a) => a -> a -> a -> Bool
isIsosceles x y z = (length . filter id) (map (uncurry (==)) (pairs x y z)) == 1

triangleType :: (Num a, Eq a, Ord a) => a -> a -> a -> TriangleType
triangleType x y z
  | isIllegal x y z     = Illegal
  | isEquilateral x y z = Equilateral
  | isIsosceles x y z   = Isosceles
  | isScalene x y z     = Scalene
