module Compressor
    ( doCompressor,
    ) where

import Parsing ( getN, getL, getF, getError, doPars )
import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitFailure), exitWith )
import Data.List ( sort )

range :: (Int, Int, Int) -> (Int, Int, Int) -> Float
range (a, b, c) (d, e, f) = sqrt((fromIntegral d - fromIntegral a)**2 + (fromIntegral e - fromIntegral b)**2 + (fromIntegral f - fromIntegral c)**2)

getRandom :: Int -> [(Int, Int, Int)]
getRandom n = if n <= 32 then take n [(30, 162, 249), (17, 144, 118), (111, 76, 220), (146, 124, 92), (69, 9, 35), (85, 52, 43), (70, 78, 80), (68, 53, 28), (184, 234, 142), (156, 253, 64), (196, 143, 219), (73, 236, 123), (88, 72, 251), (81, 34, 215), (221, 188, 126), (95, 194, 3), (141, 1, 171), (191, 172, 185), (244, 152, 242), (98, 218, 45), (240, 160, 113), (129, 25, 37), (238, 31, 108), (246, 200, 158), (167, 122, 248), (133, 190, 91), (103, 150, 211), (130, 93, 230), (39, 67, 115), (136, 105, 107), (21, 175, 189), (217, 36, 149)] else getRandom 32

getRange :: (Int, Int, Int) -> [(Int, Int, Int)] -> Int -> [(Float, Int)]
getRange _ [] _ = []
getRange (a, b, c) ((d, e, f) : xs) nb = (range (a, b, c) (d, e, f), nb) : getRange (a, b, c) xs (nb + 1)

getClose :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int, Int)]
getClose [] _ = []
getClose ((a, b, c) : xs) ((d, e, f) : yz) = do
    let ranges = getRange (a, b, c) ((d, e, f) : yz) 0
    let id = getLow ranges (255, 255)
    (a, b, c, id) : getClose xs ((d, e, f) : yz)

getLow :: [(Float, Int)] -> (Float, Int) -> Int
getLow [] (a, b) = b
getLow ((a, b) : xs) (c, d) = if a < c then getLow xs (a, b) else getLow xs (c, d)

getA :: ([(Int, Int, Int, Int)], [(Int, Int, Int)]) -> [(Int, Int, Int, Int)]
getA (a, b) = a

getB :: ([(Int, Int, Int, Int)], [(Int, Int, Int)]) -> [(Int, Int, Int)]
getB (a, b) = b

get1st :: [[String]] -> [String]
get1st [] = []
get1st ((x:xs) : z) = x : get1st z

get2nd :: [[String]] -> [String]
get2nd [] = []
get2nd ((x:xs) : z) = unwords xs : get2nd z

isUnderL :: [(Float, Int)] -> Float -> Bool
isUnderL [] _ = True
isUnderL ((a, b) : xs) l = (a <= l) && isUnderL xs l

getAverage :: [(Int, Int, Int, Int)] -> Int -> Int -> (Int, Int, Int) -> (Int, Int, Int)
getAverage [] n nb (aa, bb, cc) = if nb /= 0 then (fromIntegral (aa `div` nb), fromIntegral (bb `div` nb), fromIntegral (cc `div` nb)) else (0, 0, 0)
getAverage ((a, b, c, d) : xs) n nb (aa, bb, cc) = if d == n then getAverage xs n (nb + 1) (aa + a, bb + b, cc + c) else getAverage xs n nb (aa, bb, cc)

getAllAverage :: [(Int, Int, Int, Int)] -> Int -> Int -> [(Int, Int, Int)]
getAllAverage xs nb n = if nb == n then [] else getAverage xs nb 0 (0, 0, 0) : getAllAverage xs (nb + 1) n

getNRange :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> Int -> [(Float, Int)]
getNRange [] [] _ = []
getNRange (nx:nxs) (mean:averages) nb = (range nx mean, nb) : getNRange nxs averages (nb + 1)

doKmeans :: Float -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> ([(Int, Int, Int, Int)], [(Int, Int, Int)])
doKmeans l xs nxs = do
    let tab = getClose xs nxs
    let averages = getAllAverage tab 0 (length nxs)
    if isUnderL (getNRange nxs averages 0) l then (tab, nxs) else doKmeans l xs averages

printAll :: [(Int, Int, Int, Int)] -> [String] -> [(Int, Int, Int, Int)] -> [(Int, Int, Int)] -> [String] -> Int -> Bool -> IO ()
printAll xx yy ((a, b, c, d) : xs) y (z : zs) n inc | not inc && n < length y = putStr ("--\n" ++ show (y!!n) ++ "\n-\n") >> printAll xx yy ((a, b, c, d) : xs) y (z : zs) n True
                                                    | inc && xs /= [] = if d == n then putStr (z ++ " " ++ show (a, b, c) ++ "\n") >> printAll xx yy xs y zs n True else printAll xx yy xs y zs n True
                                                    | inc && null xs = if d == n then putStr (z ++ " " ++ show (a, b, c) ++ "\n") >> printAll xx yy xx y yy (n + 1) False else printAll xx yy xx y yy (n + 1) False
                                                    | otherwise = pure ()

doCompressor :: IO ()
doCompressor = do
    args <- getArgs
    if (length args == 6) && not (getError (getN args, getL args, getF args)) then return () else exitWith (ExitFailure 84)
    myfile <- readFile (getF args)
    let rgb = get2nd (map words (lines myfile))
    let pos = get1st (map words (lines myfile))
    let end = doKmeans (getL args) (doPars rgb) (getRandom (getN args))
    printAll (getA end) pos (getA end) (getB end) pos 0 False