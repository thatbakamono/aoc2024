module Main where

import Control.Monad (replicateM)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

parseLine :: String -> Maybe (Int, [Int])
parseLine line =
  case span (/= ':') line of
    (keyStr, ':' : rest) -> do
      key <- readMaybe keyStr :: Maybe Int
      let valueStrs = words rest
      values <- mapM (readMaybe :: String -> Maybe Int) valueStrs
      return (key, values)
    _ -> Nothing

parseInput :: String -> [(Int, [Int])]
parseInput input = catMaybes $ map parseLine (lines input)

concatNumbers :: Int -> Int -> Int
concatNumbers a b = a * 10 ^ numDigits b + b
  where
    numDigits n
      | n == 0 = 1
      | otherwise = floor (logBase 10 (fromIntegral n)) + 1

generateCombinations :: [Int] -> [[Int -> Int -> Int]]
generateCombinations nums = replicateM (length nums - 1) [(+), (*), concatNumbers]

applyOperations :: [Int] -> [Int -> Int -> Int] -> Int
applyOperations (x : xs) ops = foldl (\acc (op, n) -> op acc n) x (zip ops xs)
applyOperations _ _ = error "List of numbers must not be empty"

generateResults :: [Int] -> [Int]
generateResults nums =
  [applyOperations nums ops | ops <- generateCombinations nums]

isValid :: (Int, [Int]) -> Bool
isValid (testValue, numbers) = testValue `elem` generateResults numbers

main :: IO ()
main = do
  content <- readFile "input.txt"

  let input = parseInput content
  let validInput = filter isValid input
  let result = sum (map fst validInput)

  print result
