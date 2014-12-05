{-
Name: Brent McIvor
-}

module HW01 where

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

toDigits :: Integer -> [Integer]
toDigits n 
   | n < 1    = []
   | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = reverse (double2nd (reverse (x:y:zs))) 

double2nd :: [Integer] -> [Integer]
double2nd [] = []
double2nd (x:[]) = [x]
double2nd (x:y:zs) = x : double y : double2nd zs 

double :: Integer -> Integer
double n = n * 2

--sums all digits in a list of Integers
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:y:zs) = sumAllDigits x + sumAllDigits y +  sumDigits zs

--sums all of the digits in a single Integer
sumAllDigits :: Integer -> Integer
sumAllDigits n
   | n < 1 = 0
   | otherwise = lastDigit n + sumAllDigits(dropLastDigit n)

validate :: Integer -> Bool
validate n = (sumDigits  (doubleEveryOther (toDigits n))) `mod` 10 == 0