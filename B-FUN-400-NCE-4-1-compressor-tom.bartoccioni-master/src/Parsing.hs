module Parsing
    ( isInt, isFloat,
    getN, getL,
    getF, getError,
    doPars ) where

import Data.Char ( isDigit )

strToTriple :: [String] -> (Int, Int, Int)
strToTriple (x : xs : xy : _) = (read x, read xs, read xy)

rmPunctuation :: String -> String
rmPunctuation [] = []
rmPunctuation (x : xs) | x == ',' = ' ' : rmPunctuation xs
                     | x == '(' || x == ')' = rmPunctuation xs
                     | x /= ',' = x : rmPunctuation xs

doPars :: [String] -> [(Int, Int, Int)]
doPars = map (strToTriple . words . rmPunctuation)

isInt :: [Char] -> Bool
isInt [] = True
isInt y
    | head y == '-' = isInt (tail y)
    | isDigit (head y) = isInt (tail y)
    | otherwise = False

isFloat :: [Char] -> Bool
isFloat [] = True
isFloat y
    | head y == '-' = isFloat (tail y)
    | head y == '.' = isFloat (tail y)
    | isDigit (head y) = isFloat (tail y)
    | otherwise = False

getN :: [String] -> Int
getN [] = 84
getN ("-n":x:xs) = if isInt x then read x else 84
getN (x:xs) = getN xs

getL :: [String] -> Float
getL [] = 84
getL ("-l":x:xs) = if isFloat x then read x :: Float else 84
getL (x:xs) = getL xs

getF :: [String] -> String
getF [] = "84"
getF ("-f":x:xs) = x
getF (x:xs) = getF xs

getError :: (Int, Float, String) -> Bool 
getError (a, b, c) = a == 84 || b == 84.0 || c == "84"