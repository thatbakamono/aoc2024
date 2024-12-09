module Main where

import Control.Exception (Exception, throw)
import Data.List (isPrefixOf)
import Text.Regex.TDFA

data InvalidInstruction = InvalidInstruction String
  deriving (Show)

instance Exception InvalidInstruction

process :: (Bool, Int) -> [String] -> (Bool, Int)
process (is_enabled, result) matches =
  case () of
    _ | "mul" `isPrefixOf` head matches -> (is_enabled, result + if is_enabled then (read :: String -> Int) (matches !! 1) * (read :: String -> Int) (matches !! 2) else 0)
    _ | "don't" `isPrefixOf` head matches -> (False, result)
    _ | "do" `isPrefixOf` head matches -> (True, result)
    _ -> throw (InvalidInstruction (head matches))

main :: IO ()
main = do
  content <- readFile "input.txt"

  let pattern = "do\\(\\)|don't\\(\\)|mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

  let result = snd (foldl process (True, 0) (content =~ pattern :: [[String]]))

  print result
