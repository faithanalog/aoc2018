module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Char
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)

data Point = Point
  { px :: Int
  , py :: Int
  } deriving (Eq, Read, Show, Ord)

parsePoint :: String -> Point
parsePoint str = Point (read x) (read y)
  where
    (x, str') = span isDigit str
    str'' = drop 2 str'
    (y, _) = span isDigit str''

parsePoints :: String -> Set Point
parsePoints = Set.fromList . fmap parsePoint . lines

manhattanDistance :: Point -> Point -> Int
manhattanDistance p0 p1 = abs (px p0 - px p1) + abs (py p0 - py p1)

gridBounds :: Set Point -> (Point, Point)
gridBounds ps = (Point x0 y0, Point x1 y1)
  where
    x0 = minimum (Set.map px ps)
    y0 = minimum (Set.map py ps)
    x1 = maximum (Set.map px ps)
    y1 = maximum (Set.map py ps)

gridPoints :: Set Point -> [Point]
gridPoints ps = [Point x y | x <- [x0 .. x1], y <- [y0 .. y1]]
  where
    (Point x0 y0, Point x1 y1) = gridBounds ps

pointOwners :: Set Point -> Point-> Set Point
pointOwners targets point =
  case Map.findMin potentialOwners of
    (_, owners) -> owners
  where
    potentialOwners =
      Map.fromListWith
        (<>)
        [(manhattanDistance point p, Set.singleton p) | p <- Set.toList targets]

-- Trace the grid space directly outside the valid grid. Any points that own
-- these points have infinite area
infinitePoints :: Set Point -> Set Point
infinitePoints targets = foldMap (pointOwners targets) outline
  where
    (Point x0 y0, Point x1 y1) = gridBounds targets
    top = [Point x (y0 - 1) | x <- [x0 - 1 .. x1 + 1]]
    bottom = [Point x (y1 + 1) | x <- [x0 - 1 .. x1 + 1]]
    left = [Point (x0 - 1) y | y <- [y0 .. y1]]
    right = [Point (x1 + 1) y | y <- [y0 .. y1]]
    outline = Set.fromList (top ++ bottom ++ left ++ right)

part1 :: Set Point -> Int
part1 targets = maximum (Map.elems areas)
  where
    (Point x0 y0, Point x1 y1) = gridBounds targets
    initialMap = Map.fromSet (const 0) targets
    tallyPoint p =
      case pointOwners targets p of
        xs | length xs > 1 -> id
        xs -> Map.adjust (+ 1) (head (Set.toList xs))
    areas =
      Map.withoutKeys
        (foldr
           (.)
           id
           [tallyPoint p | p <- gridPoints targets]
           initialMap)
        (infinitePoints targets)

part2 :: Set Point -> Int
part2 targets =
  length [regionSize p | p <- gridPoints targets, regionSize p < 10000]
  where
    regionSize p = foldr (\x total -> manhattanDistance p x + total) 0 targets

main :: IO ()
main = do
  input <- readFile "input"
  let targets = parsePoints input
  putStr "Part 1: "
  print (part1 targets)
  putStr "Part 2: "
  print (part2 targets)
