module Main where

import Data.Char (isSpace)
import Data.List (sort)
import System.IO
import Text.Read (readMaybe)

parse :: String -> Maybe (Int, Int)
parse line =
  let (first, rest) = break isSpace line
      part1 = readMaybe first :: Maybe Int
      part2 = readMaybe (dropWhile isSpace rest) :: Maybe Int
   in case (part1, part2) of
        (Just x, Just y) -> Just (x, y)
        _ -> Nothing

distance :: Int -> Int -> Int
distance lhs rhs = abs (lhs - rhs)

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    content <- hGetContents handle

    let allLines = lines content
    let parsedLines = map parse allLines
    let validLines = [(firstLocationId, secondLocationId) | Just (firstLocationId, secondLocationId) <- parsedLines]

    let (locationIds1, locationIds2) = unzip validLines

    let sortedLocationIds1 = sort locationIds1
        sortedLocationIds2 = sort locationIds2

    print $ sum [distance firstLocationId secondLocationId | (firstLocationId, secondLocationId) <- zip sortedLocationIds1 sortedLocationIds2]