module Base (rebase) where

toBaseTen :: Integral a => a -> [a] -> a
toBaseTen _ [] = 0
toBaseTen base digits = (head digits) * (base ^ ((length digits) - 1)) + toBaseTen base (tail digits)

fromBaseTen :: Integral a => a -> a -> [a]
fromBaseTen base x = reverse $ f base x []
  where f base x digits
          | x == 0 = []
          | x < base = x : digits
          | otherwise = r : f base q digits
            where (q,r) = quotRem x base

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits
  | inputBase <= 1 = Nothing
  | outputBase <= 1 = Nothing
  | any (<0) inputDigits = Nothing
  | any (>= inputBase) inputDigits = Nothing
  | inputDigits == [] = Just []
  | otherwise = Just $ (fromBaseTen outputBase . toBaseTen inputBase) inputDigits