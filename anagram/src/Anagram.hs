module Anagram (anagramsFor) where

import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as M

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter (checkAnagrams word)

checkAnagrams :: String -> String -> Bool
checkAnagrams a b
  | map toUpper a == map toUpper b = False
  | countChars a == countChars b = True
  | otherwise = False

countChars :: String -> Map Char Int
countChars = go M.empty
  where
    go :: Map Char Int -> String -> Map Char Int
    go m [] = m
    go m (x : xs) = go (M.insertWith (+) (toUpper x) 1 m) xs
