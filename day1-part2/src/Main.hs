module Main where

import Data.Char (isSpace)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
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

countOccurrences :: (Eq a, Hashable a) => [a] -> HashMap a Int
countOccurrences = foldl' (\acc x -> HashMap.insertWith (+) x 1 acc) HashMap.empty

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    content <- hGetContents handle

    let allLines = lines content
    let parsedLines = map parse allLines
    let validLines = [(firstLocationId, secondLocationId) | Just (firstLocationId, secondLocationId) <- parsedLines]

    let (locationIds1, locationIds2) = unzip validLines

    let uniqueSortedLocationIds1 = Set.toList (Set.fromList locationIds1)
    let occurrencesOfLocationIds2 = countOccurrences locationIds2

    let similarityScores = map (\locationId -> locationId * fromMaybe 0 (HashMap.lookup locationId occurrencesOfLocationIds2)) uniqueSortedLocationIds1

    print (sum similarityScores)