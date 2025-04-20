module Grains (square, total) where

square :: Integer -> Maybe Integer
square x
  | x <= 0 = Nothing
  | x > 64 = Nothing
  | otherwise = Just $ 2 ^ (x - 1)

total :: Integer
total = 2 ^ (64 :: Integer) - 1
