module Homework01 (
    toDigits,
    toDigitsRev
) where

-- toDigits
toDigits :: Integer -> [Integer]
toDigits num
  | num <= 0 = []
  | otherwise = toDigits ( num `div` 10 ) ++ [ num `mod` 10 ]

-- toDigitRev
toDigitsRev :: Integer -> [Integer]
toDigitRev = reverse . toDigits

