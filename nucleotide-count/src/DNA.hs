module DNA (nucleotideCounts, Nucleotide (..)) where

import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map.Map Nucleotide Int)
nucleotideCounts str = go str (Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)])
  where
    go :: String -> Map.Map Nucleotide Int -> Either String (Map.Map Nucleotide Int)
    go (x : xs) m
      | x == 'A' = go xs (Map.adjust succ A m)
      | x == 'C' = go xs (Map.adjust succ C m)
      | x == 'G' = go xs (Map.adjust succ G m)
      | x == 'T' = go xs (Map.adjust succ T m)
      | otherwise = Left "invalid"
    go [] m = Right m
