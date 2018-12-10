{-# LANGUAGE DeriveTraversable #-}
module Main where

-- dependencies: vector, mtl

import Control.Monad.State.Strict
import Data.Char
import Data.Monoid
import qualified Data.Vector as V
import Data.Vector (Vector, (!))

data Tree a =
  Node a
       (Vector (Tree a))
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)
type Metadata = Vector Int
type LicenseTree = Tree Metadata

parseLicenseTree :: Vector Int -> LicenseTree
parseLicenseTree = evalState subTree
  where
    subTree :: State (Vector Int) LicenseTree
    subTree = do
      (nChildren, nMetadata) <-
        state (\bytes -> ((bytes ! 0, bytes ! 1), V.drop 2 bytes))
      children <- V.replicateM nChildren subTree
      metadata <- state (V.splitAt nMetadata)
      pure (Node metadata children)

readNumbers :: String -> Vector Int
readNumbers = V.unfoldr psi
  where
    psi str =
      case span isDigit str of
        ([], _) -> Nothing
        (num, leftover) -> Just (read num, dropWhile (not . isDigit) leftover)

nodeValue :: LicenseTree -> Int
nodeValue (Node metadata children)
  | V.null children = sum metadata
  | otherwise =
    let childValues = fmap nodeValue children
        indexMeta meta
          | meta <= 0 || meta > V.length childValues = 0
          | otherwise = childValues ! (meta - 1)
     in getSum (foldMap (Sum . indexMeta) metadata)

main :: IO ()
main = do
  input <- readFile "input"
  let licenseTree = parseLicenseTree (readNumbers input)
  putStr "Part 1: "
  print (sum (fmap sum licenseTree))
  putStr "Part 2: "
  print (nodeValue licenseTree)
