module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace

data Tape a =
  Tape [a]
       Int
       a
       [a]
  deriving (Eq, Read, Show, Ord)

left :: Tape a -> Tape a
left (Tape [] i x rs) = Tape [] i x rs
left (Tape (l:ls) i x rs) = Tape ls (i - 1) l (x:rs)

right :: Tape a -> Tape a
right (Tape ls i x []) = Tape ls i x []
right (Tape ls i x (r:rs)) = Tape (x:ls) (i + 1) r rs

goto :: Int -> Tape a -> Tape a
goto idx (Tape ls i x rs) =
  case compare idx i of
    EQ -> Tape ls i x rs
    LT -> goto idx (left (Tape ls i x rs))
    GT -> goto idx (right (Tape ls i x rs))

type Pot = Tape Char

type GrowthRules = Map (Tape Char) Char

potValue :: Pot -> Int
potValue (Tape _ i '#' _) = i
potValue (Tape _ _ '.' _) = 0

parseGrowthRule :: String -> (Tape Char, Char)
parseGrowthRule (l1:l0:x:r0:r1:' ':'=':'>':' ':nxt:_) = (Tape [l0, l1] 0 x [r0, r1], nxt)

parseGrowthRules :: [String] -> GrowthRules
parseGrowthRules lns = Map.fromList (fmap parseGrowthRule lns)

parseInitialState :: String -> Window
parseInitialState ln =
  let pots = drop (length "initial state: ") ln
   in Window
        { winTape = Tape (repeat '.') 0 (head pots) (tail pots ++ repeat '.')
        , winLeft = 0
        , winRight = length pots - 1
        , winGen = 0
        , winGliders = []
        }

nextPotState :: GrowthRules -> Pot -> Char
nextPotState rules (Tape ls i x rs) =
  let lsThatMatter = take 2 ls
      rsThatMatter = take 2 rs
      Just nextState = Map.lookup (Tape lsThatMatter 0 x rsThatMatter) rules
   in nextState

updatePot :: GrowthRules -> Pot -> Pot
updatePot rules pots =
  let x = nextPotState rules pots
      ls = fmap (nextPotState rules) (tail (iterate left pots))
      rs = fmap (nextPotState rules) (tail (iterate right pots))
      Tape _ i _ _ = pots
   in Tape ls i x rs

data Glider = Glider
  { gliderSpawnPoint :: Integer
  , gliderSpawnGeneration :: Integer
  , gliderLen :: Integer
  } deriving (Eq, Read, Show)

gliderValue :: Integer -> Glider -> Integer
gliderValue now (Glider s g l) =
  (s + (now - g)) * l +
  (if l == 3
     then 3
     else 6)

data Window = Window
  { winTape :: Pot
  , winLeft :: Int
  , winRight :: Int
  , winGen :: Integer
  , winGliders :: [Glider]
  } deriving (Eq, Read, Show)

winPlants :: Window -> [Char]
winPlants win =
  let Tape ls i x rs = winTape win
      wls = reverse (take (i - winLeft win) ls)
      wrs = take (winRight win - i) rs
   in wls ++ (x : wrs)

winPots :: Window -> [Pot]
winPots win =
  let t@(Tape _ i _ _) = winTape win
      wls = reverse (take (i - winLeft win) (tail (iterate left t)))
      wrs = take (winRight win - i) (tail (iterate right t))
   in wls ++ (t : wrs)

winEmpty :: Window -> Bool
winEmpty win = notElem '#' (winPlants win)

updateWindow :: GrowthRules -> Window -> Window
updateWindow gr win =
  let t = updatePot gr (winTape win)
      g = winGen win + 1
      l =
        case goto (winLeft win) t of
          Tape ('.':_) i '.' _ -> i + 1
          Tape ('#':_) i _ _ -> i - 2
          Tape _ i _ _ -> i
      (r, glider) =
        case goto (winRight win) t of
          Tape ('#':'.':'.':'.':_) i '#' ('#':'.':'.':'.':_) ->
            (i - 5, [Glider (toInteger i - 1) g 3])
          Tape ('#':'#':'.':'.':'.':_) i '#' ('#':'.':'.':'.':_) ->
            (i - 6, [Glider (toInteger i - 2) g 4])
          Tape ('.':_) i '.' _ -> (i - 1, [])
          Tape _ i _ ('#':_) -> (i + 2, [])
          _ -> (winRight win, [])
   in Window
        { winTape = goto ((l + r) `div` 2) t
        , winLeft = min l r
        , winRight = max l r
        , winGen = g
        , winGliders = glider ++ winGliders win
        }

winValue :: Window -> Integer
winValue win =
  sum (fmap (toInteger . potValue) (winPots win)) +
  sum (fmap (gliderValue (winGen win)) (winGliders win))

runGenerations :: Integer -> GrowthRules -> Window -> Window
runGenerations n gr win
  | winGen win == n = win
  | winEmpty win = win { winGen = n }
  | otherwise = runGenerations n gr (updateWindow gr win)

part1 :: GrowthRules -> Window -> Integer
part1 gr win = winValue (runGenerations 20 gr win)

part2 :: GrowthRules -> Window -> Integer
part2 gr win = winValue (runGenerations 50000000000 gr win)

main :: IO ()
main = do
  input <- readFile "input"
  let lns = lines input
      initWin = parseInitialState (head lns)
      rules = parseGrowthRules (drop 2 lns)
  putStr "Part 1: "
  print (part1 rules initWin)
  putStr "Part 2: "
  print (part2 rules initWin)

