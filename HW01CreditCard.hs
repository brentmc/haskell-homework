{-
Name: Brent McIvor
-}

module HW01 where

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

--converts the given Integer to a list of its digits
toDigits :: Integer -> [Integer]
toDigits n 
   | n < 1    = []
   | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

{-
doubles every second digit beginning from the right
i.e. the last digit is unchanged, 
the second last is doubled, 
the third last is unchanged
-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (xs) = reverse (double2nd (reverse (xs))) 

--doubles every second element in the given list
double2nd :: [Integer] -> [Integer]
double2nd [] = []
double2nd (x:[]) = [x]
double2nd (x:y:zs) = x : double y : double2nd zs 

double :: Integer -> Integer
double n = n * 2

--sums all digits in a list of Integers
sumAllDigitsInList :: [Integer] -> Integer
sumAllDigitsInList [] = 0
sumAllDigitsInList [x] = x
sumAllDigitsInList (x:ys) = sumDigitsInInteger x + sumAllDigitsInList ys

--sums all of the digits in a single Integer
sumDigitsInInteger :: Integer -> Integer
sumDigitsInInteger n
   | n < 1 = 0
   | otherwise = lastDigit n + sumDigitsInInteger(dropLastDigit n)

--determines if the given Integer is a valid credit card number
validate :: Integer -> Bool
validate n = (sumAllDigitsInList  (doubleEveryOther (toDigits n))) `mod` 10 == 0