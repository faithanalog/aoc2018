module Main where

import Data.Char
import Data.List

invertCase :: Char -> Char
invertCase c
  | isLower c = toUpper c
  | isUpper c = toLower c
  | otherwise = c

react :: String -> String
react = foldr phi []
  where
    phi a (b:xs)
      | a == invertCase b = xs
      | otherwise = a : b : xs
    phi a [] = [a]

removeElement :: Char -> String -> String
removeElement element = filter (\c -> c /= element && c /= invertCase element)

optimizedReaction :: String -> String
optimizedReaction polymer =
  head (sortOn length [react (removeElement e polymer) | e <- ['a' .. 'z']])


main :: IO ()
main = do
  input <- fmap (filter isLetter) (readFile "input")
  
  putStr "Part 1: "
  print (length (react input))

  putStr "Part 2: "
  print (length (optimizedReaction input))
