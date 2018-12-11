module Main where

import qualified Data.Vector.Unboxed as V
import Data.List
import Data.Function

serialNumber :: Int
serialNumber = 4172

gridSize :: Int
gridSize = 300

powerLevelCalc :: Int -> Int -> Int
powerLevelCalc x y =
  let rackID = x + 10
   in (((((rackID * y) + serialNumber) * rackID) `mod` 1000) `div` 100) - 5

powerCache :: V.Vector Int
powerCache =
  V.generate
    (gridSize * gridSize)
    (\i ->
       let x = i `mod` gridSize + 1
           y = i `div` gridSize + 1
        in powerLevelCalc x y)

powerCacheTransposed :: V.Vector Int
powerCacheTransposed =
  V.generate
    (gridSize * gridSize)
    (\i ->
       let x = i `mod` gridSize
           y = i `div` gridSize
           j = y + x * gridSize
        in powerCache V.! j)

powerCacheIndex :: Int -> Int -> Int
powerCacheIndex x y = (x - 1) + (y - 1) * gridSize

powerSquareDelta :: Int -> Int -> Int -> Int
powerSquareDelta x y size =
  let bottomRow =
        V.sum (V.slice (powerCacheIndex x (y + size - 1)) size powerCache)
      rightColumn =
        V.sum (V.slice (powerCacheIndex y (x + size - 1)) (size - 1) powerCacheTransposed)
   in bottomRow + rightColumn

applyDelta :: Int -> V.Vector Int -> V.Vector Int
applyDelta size prevGrid =
  let dimms = gridSize - size + 1
      grid =
        V.concat
          [V.slice (y * (dimms + 1)) dimms prevGrid | y <- [0 .. dimms - 1]]
      delta i =
        let x = (i `mod` dimms) + 1
            y = (i `div` dimms) + 1
         in powerSquareDelta x y size
   in V.imap (\i v -> delta i + v) grid

powerSquareGrids :: [V.Vector Int]
powerSquareGrids =
  let grids = powerCache : zipWith applyDelta [2 ..] grids
   in take gridSize grids

strongestSquareInGrid :: Int -> V.Vector Int -> (Int, Int, Int, Int)
strongestSquareInGrid size grid =
  let idx = V.maxIndex grid
      x = (idx `mod` (gridSize - size + 1)) + 1
      y = (idx `div` (gridSize - size + 1)) + 1
   in (size, x, y, grid V.! idx)

part1 :: (Int, Int)
part1 =
  let (_, x, y, _) = strongestSquareInGrid 3 (powerSquareGrids !! 2)
   in (x, y)

part2 :: (Int, Int, Int)
part2 =
  let pred = on compare (\(_, _, _, p) -> p)
      (size, x, y, _) =
        maximumBy pred (zipWith strongestSquareInGrid [1 ..] powerSquareGrids)
   in (size, x, y)


main :: IO ()
main = do
  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
