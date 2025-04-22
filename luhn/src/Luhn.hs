module Luhn (isValid) where

import Data.Char (digitToInt, isDigit, isSpace)

sumDigits :: (Integral a) => [a] -> Bool -> a
sumDigits [] _ = 0
sumDigits (x : xs) shouldDouble
  | shouldDouble = double' x + sumDigits xs (not shouldDouble)
  | otherwise = x + sumDigits xs (not shouldDouble)
  where
    double' :: (Integral a) => a -> a
    double' a
      | a * 2 > 9 = a * 2 - 9
      | otherwise = a * 2

checkValid :: Char -> Bool
checkValid x = isDigit x || isSpace x

isValid :: String -> Bool
isValid input
  | not (all checkValid input) = False
  | length converted <= 1 = False
  | otherwise = value `mod` 10 == 0
  where
    filtered = filter isDigit input
    converted = map digitToInt filtered
    value = sumDigits converted $ even (length converted)
