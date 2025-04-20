module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

shouldSkip :: Char -> Bool
shouldSkip c
  | isAlpha c = False
  | c `elem` " -" = False
  | otherwise = True

isDelimeter :: Char -> Bool
isDelimeter ' ' = True
isDelimeter '-' = True
isDelimeter _ = False

reduce :: [String] -> String -> String -> [String]
reduce res [] [] = reverse res
reduce res word [] = reduce (reverse word : res) [] []
reduce res word (x : xs)
  | shouldSkip x = reduce res word xs
  | isDelimeter x = reduce (reverse word : res) [] xs
  | otherwise = reduce res (x : word) xs

words' :: String -> [String]
words' = reduce [] []

abbreviate :: String -> String
abbreviate input = res
  where
    res = concatMap initials parts
    parts = filter (/= "") $ words' input

initials :: String -> String
initials [] = []
initials (x : xs) = toUpper x : rest
  where
    rest =
      if not (all isUpper xs)
        then filter isUpper xs
        else []
