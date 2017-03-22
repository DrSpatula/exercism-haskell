module PrimeFactors (primeFactors) where

import Data.List (sort)
import Data.List.Ordered (minus)

-- primesTo from https://wiki.haskell.org/Prime_numbers#Sieve_of_Eratosthenes
-- had my own version in here and working, but was hoping this one was more effecient
primesTo :: Integer -> [Integer]
primesTo n = 2 : sieve [3,5..n]
  where sieve (p:xs)
          | p*p > n = p : xs
          | otherwise = p : sieve (xs `minus` map (p*) [p, p+2..])

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = sort $ f (primesTo n) [] n
  where 
    f :: [Integer] -> [Integer] -> Integer -> [Integer]
    f primes factors x
      | p == x = x : factors
      | x `mod` p == 0 = f primes (p : factors) (x `div` p)
      | x `mod` p /= 0 = f (tail primes) factors x
        where p = head primes