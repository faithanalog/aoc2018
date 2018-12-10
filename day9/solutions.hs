{-# LANGUAGE TemplateHaskell #-}
module Main where

-- Dependencies: lens, mtl, containers

import Control.Lens
import Control.Monad.State.Lazy
import Data.List hiding (insert)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

data Circle a =
  Circle Int
         (Seq a)
  deriving (Eq, Read, Show)

singleton :: a -> Circle a
singleton x = Circle 0 (Seq.singleton x)

-- Move the read/write head counter-clockwise
goCounterClockwise :: Circle a -> Circle a
goCounterClockwise = goCounterClockwiseN 1

goCounterClockwiseN :: Int -> Circle a -> Circle a
goCounterClockwiseN n = goClockwiseN (-n)

goClockwise :: Circle a -> Circle a
goClockwise = goClockwiseN 1

goClockwiseN :: Int -> Circle a -> Circle a
goClockwiseN n (Circle i xs) = Circle ((i + n + length xs) `mod` length xs) xs

insert :: a -> Circle a -> Circle a
insert x (Circle i xs) = Circle i (Seq.insertAt i x xs)

remove :: Circle a -> Circle a
remove (Circle i xs) =
  Circle (i `mod` length (Seq.deleteAt i xs)) (Seq.deleteAt i xs)

readHead :: Circle a -> a
readHead (Circle i xs) = Seq.index xs i

numPlayers :: Int
numPlayers = 493

lastMarbleOne :: Int
lastMarbleOne = 71863

lastMarbleTwo :: Int
lastMarbleTwo = lastMarbleOne * 100

type Marble = Int

type Players = Seq Int

data GameState = GameState
  { _gameMarble :: Int
  , _gameCircle :: Circle Marble
  }

makeLenses ''GameState

newGameState :: GameState
newGameState = GameState 0 (singleton 0)

type Game a = StateT GameState Maybe a

playMarble :: Marble -> Game Int
playMarble m
  | m `mod` 23 == 0 = do
    gameCircle %= goCounterClockwiseN 7
    x <- uses gameCircle readHead
    gameCircle %= remove
    pure (m + x)
  | otherwise = do
    gameCircle %= (insert m . goClockwiseN 2)
    pure 0

playUntilScore :: Int -> Game (Players -> Players)
playUntilScore lastMarble = do
  gameMarble += 1
  m <- use gameMarble
  if m > lastMarble
    then lift Nothing
    else do
      score <- playMarble m
      let player = (m - 1) `mod` numPlayers
      if score == 0
        then playUntilScore lastMarble
        else pure (Seq.adjust' (+ score) player)

playFullGame :: Int -> Players
playFullGame lastMarble =
  foldl'
    (&)
    (Seq.replicate numPlayers 0)
    (unfoldr (runStateT (playUntilScore lastMarble)) newGameState)

main :: IO ()
main = do
  putStr "Part 1: "
  print (maximum (playFullGame lastMarbleOne))
  putStr "Part 2: "
  print (maximum (playFullGame lastMarbleTwo))
