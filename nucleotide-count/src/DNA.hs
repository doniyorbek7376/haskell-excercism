module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts str
  | isValid = Right $ count str
  | otherwise = Left "invalid"
  where
    isValid = all (`elem` "ACGT") str

count :: String -> Map Nucleotide Int
count str = M.fromListWith (+) [(read [c], 1) | c <- str]
