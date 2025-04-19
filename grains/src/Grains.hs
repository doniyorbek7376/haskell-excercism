module Grains (square, total) where

import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square x
  | x <= 0 = Nothing
  | x > 64 = Nothing
  | x == 1 = Just 1
  | otherwise = Just $ 2 ^ (x - 1)

total :: Integer
total = sumMaybe $ map square [1 .. 64]

sumMaybe :: [Maybe Integer] -> Integer
sumMaybe [] = 0
sumMaybe (Nothing : xs) = sumMaybe xs
sumMaybe (x : xs) = fromJust x + sumMaybe xs
