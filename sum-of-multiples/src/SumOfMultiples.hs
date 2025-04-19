module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = res
  where
    divisable :: Integer -> Integer -> Bool
    divisable _ 0 = False
    divisable a b = a `mod` b == 0
    ok :: Integer -> Bool
    ok x = any (divisable x) factors
    res = sum (filter ok [1 .. limit - 1])
