module Raindrops (convert) where

pling :: Integral a => a -> Maybe String
pling x
  | x `mod` 3 == 0 = Just "Pling"
  | otherwise      = Nothing

plang :: Integral a => a -> Maybe String
plang x
  | x `mod` 5 == 0 = Just "Plang"
  | otherwise      = Nothing

plong :: Integral a => a -> Maybe String
plong x
  | x `mod` 7 == 0 = Just "Plong"
  | otherwise      = Nothing

convert :: (Show a, Integral a) => a -> String
convert x = maybe (show x) id (mconcat [(pling x), (plang x), (plong x)])
