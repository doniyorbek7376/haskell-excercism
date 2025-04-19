module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA [] = Right []
toRNA x
  | not (null invalid) = Left (head invalid)
  where
    invalid = filter (`notElem` "ATCG") x
toRNA x = Right (map convert x)
  where
    convert :: Char -> Char
    convert 'A' = 'U'
    convert 'T' = 'A'
    convert 'G' = 'C'
    convert 'C' = 'G'
    convert c = c
