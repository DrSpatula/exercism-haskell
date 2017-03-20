module SecretHandshake (handshake) where

import Data.Bits ((.&.))

wink :: Int -> [String]
wink x
  | x .&. 1 == 1 = ["wink"]
  | otherwise = []

doubleBlink :: Int -> [String]
doubleBlink x
  | x .&. 2 == 2 = ["double blink"]
  | otherwise = []

closeYourEyes :: Int -> [String]
closeYourEyes x
  | x .&. 4 == 4 = ["close your eyes"]
  | otherwise = []

jump :: Int -> [String]
jump x
  | x .&. 8 == 8 = ["jump"]
  | otherwise = []

rev :: Int -> [String] -> [String]
rev x hs =
  case x .&. 16 of
    0  -> hs
    16 -> reverse hs

handshake :: Int -> [String]
handshake = do
  w <- wink
  d <- doubleBlink
  c <- closeYourEyes
  j <- jump
  r <- rev
  return $ (r . concat) [w, d, c, j]
