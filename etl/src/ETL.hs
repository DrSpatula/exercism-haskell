module ETL (transform) where

import qualified Data.Map as M
import Data.Char

transOne :: (a, String) -> [(Char, a)]
transOne (k, chars) = zip (map toLower chars) (repeat k)

transform :: M.Map a String -> M.Map Char a
transform legacyData = M.fromList $ concatMap transOne (M.toList legacyData)
