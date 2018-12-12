{-# LANGUAGE DeriveTraversable #-}

module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Tape a =
  Tape [a]
       a
       [a]
  deriving (Eq, Read, Show, Ord, Functor, Foldable, Traversable)

left :: Tape a -> Tape a
left (Tape [] x rs) = Tape [] x rs
left (Tape (l:ls) x rs) = Tape ls l (x:rs)

right :: Tape a -> Tape a
right (Tape ls x []) = Tape ls x []
right (Tape ls x (r:rs)) = Tape (x:ls) r rs

type Pots = Tape Char

type GrowthRules = Map (Tape Char) Char

parseGrowthRule :: String -> (Tape Char, Char)
parseGrowthRule (l1:l0:x:r0:r1:' ':'=':'>':' ':nxt:_) = (Tape [l0, l1] x [r0, r1], nxt)

parseGrowthRules :: [String] -> GrowthRules
parseGrowthRules lns = Map.fromList (fmap parseGrowthRule lns)

parseInitialState :: String -> (Pots, Int)
parseInitialState ln =
  let pots = drop (length "initial state: ") ln
   in (Tape (repeat '.') (head pots) (tail pots ++ repeat '.'), length pots)

nextPotState :: GrowthRules -> Pots -> Char
nextPotState rules (Tape ls x rs) =
  let lsThatMatter = take 2 ls
      rsThatMatter = take 2 rs
      Just nextState = Map.lookup (Tape lsThatMatter x rsThatMatter) rules
   in nextState

updatePots :: GrowthRules -> Pots -> Pots
updatePots rules pots =
  let x = nextPotState rules pots
      ls = fmap (nextPotState rules) (tail (iterate left pots))
      rs = fmap (nextPotState rules) (tail (iterate right pots))
   in Tape ls x rs

numGrowthSteps :: Int
numGrowthSteps = 20

part1 :: Int -> Pots -> Int
part1 numPots pots =
  let (significantLs, significantRs) =
        case pots of
          Tape ls x rs ->
            (take numGrowthSteps ls, x : take (numGrowthSteps + numPots) rs)
      addFilledPots pids pots =
        sum
          (zipWith
             (\pid x ->
                if x == '#'
                  then pid
                  else 0)
             pids
             pots)
   in addFilledPots [-1,-2 ..] significantLs +
      addFilledPots [0 ..] significantRs


renderPots :: Int -> Pots -> String
renderPots viewArea (Tape ls x rs) =
  reverse (take viewArea ls) ++ (x : take viewArea rs)

test :: String -> IO ()
test initialState = do
  input <- readFile "input"
  let lns = lines input
      rules = parseGrowthRules (drop 2 lns)
      -- Shift over our tape to be centered on the initial state
      pots = foldr (.) id (replicate (length initialState `div` 2) right) (Tape (repeat '.') '.' (initialState ++ repeat '.'))
      steps = take 400 (iterate (updatePots rules) pots)
  mapM_ (putStrLn . renderPots 39) steps

main :: IO ()
main = do
  input <- readFile "input"
  let lns = lines input
      (pots, numPots) = parseInitialState (head lns)
      rules = parseGrowthRules (drop 2 lns)
      updatedPots =
        foldr (.) id (replicate numGrowthSteps (updatePots rules)) pots
  putStr "Part 1: "
  print (part1 numPots updatedPots)

