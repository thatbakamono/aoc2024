module Main where

import Text.Regex.TDFA

main :: IO ()
main = do
  content <- readFile "input.txt"
  let pattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

  let results = map (\matches -> (read :: String -> Int) (matches !! 1) * (read :: String -> Int) (matches !! 2)) (content =~ pattern :: [[String]])
  let result = sum results

  print result