module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs = res
  where
    table = split . normalize $ xs
    transposed = transpose table
    res = unwords transposed

split :: String -> [String]
split input = [take' x ' ' $ drop (x * i - x) input | i <- [1 .. y]]
  where
    x = sideLength . length $ input
    y = ceiling ((fromIntegral . length $ input) / fromIntegral x :: Double)

sideLength :: (Integral a) => a -> a
sideLength x = ceiling . sqrt $ (fromIntegral x :: Float)

normalize :: String -> String
normalize = filterLetters . downcase
  where
    downcase :: String -> String
    downcase = map toLower

    filterLetters :: String -> String
    filterLetters = filter isAlphaNum

take' :: (Integral i) => i -> a -> [a] -> [a]
take' 0 _ _ = []
take' count d [] = d : take' (count - 1) d []
take' count d (x : xs) = x : take' (count - 1) d xs
