module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor input
  | silence = "Fine. Be that way!"
  | isAsking && isShouting = "Calm down, I know what I'm doing!"
  | isAsking = "Sure."
  | isShouting = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    silence = null text
    text = filter (not . isSpace) input
    letters = filter isLetter text
    isAsking = last text == '?'
    isShouting = all isUpper letters && not (null letters)
