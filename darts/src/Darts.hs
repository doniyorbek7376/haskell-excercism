module Darts (score) where

sqr :: (Num a) => a -> a
sqr x = x * x

hyper :: Float -> Float -> Float
hyper x y = sqrt (sqr x + sqr y)

score :: Float -> Float -> Int
score x y
  | radius <= 1 = 10
  | radius <= 5 = 5
  | radius <= 10 = 1
  | otherwise = 0
  where
    radius = hyper x y
