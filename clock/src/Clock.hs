module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock Int Int
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin cHour cMin = Clock cHour'' cMin'
  where
    cHour' = cHour + cMin `div` 60
    cHour'' = cHour' `mod` 24
    cMin' = cMin `mod` 60

toString :: Clock -> String
toString (Clock cHour cMin) = printf "%02d:%02d" cHour cMin

addDelta :: Int -> Int -> Clock -> Clock
addDelta dHour dMin (Clock cHour cMin) = Clock nHour' nMin'
  where
    nHour = dHour + cHour + nMin `div` 60
    nMin = dMin + cMin
    nHour' = nHour `mod` 24
    nMin' = nMin `mod` 60
