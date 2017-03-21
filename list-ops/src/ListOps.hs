module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x:xs) = z' `seq` foldl' f z' xs
  where z' = f z x

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

length :: [a] -> Int
length = foldr (\_ b -> b + 1) 0

reverse :: [a] -> [a]
reverse xs = foldl' (flip (:)) [] xs

map :: (a -> b) -> [a] -> [b]
map f = foldr (\a b -> (f a) : b) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\a b -> if p a then a : b else b) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss
