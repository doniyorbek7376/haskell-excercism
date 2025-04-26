module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify x
  | x <= 0 = Nothing
  | s == x = Just Perfect
  | s > x = Just Abundant
  | otherwise = Just Deficient
  where
    s = aliquot x

aliquot :: Int -> Int
aliquot x = sum [c | c <- [1 .. x - 1], x `mod` c == 0]
