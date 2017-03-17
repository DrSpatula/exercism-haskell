module DNA (toRNA) where

import Data.Maybe (isNothing)

complement :: Char -> Maybe String
complement 'G' = Just "C"
complement 'C' = Just "G"
complement 'T' = Just "A"
complement 'A' = Just "U"
complement  _  = Nothing

toRNA :: String -> Maybe String
toRNA xs
  | any isNothing rna = Nothing
  | otherwise = mconcat rna
  where rna = map complement xs
