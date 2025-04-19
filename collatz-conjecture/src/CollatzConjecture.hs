module CollatzConjecture (collatz) where

import Data.Maybe (fromJust, listToMaybe)

add' :: Maybe Integer -> Maybe Integer -> Maybe Integer
add' Nothing _ = Nothing
add' _ Nothing = Nothing
add' a b = listToMaybe [fromJust a + fromJust b]

collatz :: Integer -> Maybe Integer
collatz n | n <= 0 = Nothing
collatz 1 = Just 0
collatz n
  | even n = Just 1 `add'` collatz (n `div` 2)
  | otherwise = Just 1 `add'` collatz (n * 3 + 1)
