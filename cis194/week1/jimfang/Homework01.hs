module Homework01 (
    toDigits,
    toDigitsRev,
    doubleEveryOther,
    sumDigits,
    validate
) where

-- toDigits
toDigits :: Integer -> [Integer]
toDigits num
  | num <= 0 = []
  | otherwise = toDigits ( num `div` 10 ) ++ [ num `mod` 10 ]

-- toDigitRev
toDigitsRev :: Integer -> [Integer]
toDigitRev = reverse . toDigits

-- doubleEveryOther
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther [x]        = [x]
doubleEveryOther [x:y:rest] = [x: y*2] ++ doubleEveryOther rest

-- sumDigits
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap . toDigits

-- validate
tenTimes :: Integer -> Bool
tenTimes num = num `mod` 10 == 0

validate :: Integer -> Bool
validate = tenTimes . sumDigits . doubleEveryOther . toDigits 
