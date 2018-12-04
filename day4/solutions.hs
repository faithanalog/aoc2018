{-# LANGUAGE TemplateHaskell #-}
module Main where

-- Dependencies: parsec, containers, mtl, lens

import qualified Text.Parsec as P
import Control.Applicative
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict as IntMap
import Control.Lens
import Control.Monad.State.Strict
import Data.List

data Time = Time
  { timeYear :: Int
  , timeMonth :: Int
  , timeDay :: Int
  , timeHour :: Int
  , timeMinute :: Int
  } deriving (Eq, Read, Show, Ord)

type Parser = P.Parsec String ()

parseNumber :: Parser Int
parseNumber = do
  xs <- P.many1 P.digit
  pure (read xs)

parseTime :: Parser Time
parseTime = do
  _ <- P.char '['
  year <- parseNumber
  _ <- P.char '-'
  month <- parseNumber
  _ <- P.char '-'
  day <- parseNumber
  _ <- P.char ' '
  hour <- parseNumber
  _ <- P.char ':'
  minute <- parseNumber
  _ <- P.char ']'
  pure
    Time
      { timeYear = year
      , timeMonth = month
      , timeDay = day
      , timeHour = hour
      , timeMinute = minute
      }
 
data Action
  = ActionSleep
  | ActionWake
  | ActionBeginShift Int
  deriving (Eq, Read, Show, Ord)

parseActionSleep :: Parser Action
parseActionSleep = P.string "falls asleep" *> pure ActionSleep

parseActionWake :: Parser Action
parseActionWake = P.string "wakes up" *> pure ActionWake

parseActionBeginShift :: Parser Action
parseActionBeginShift = do
  _ <- P.string "Guard #"
  guardId <- parseNumber
  _ <- P.string " begins shift"
  pure (ActionBeginShift guardId)

parseAction :: Parser Action
parseAction = parseActionSleep <|> parseActionWake <|> parseActionBeginShift

data Event = Event
  { eventTime :: Time
  , eventAction :: Action
  } deriving (Eq, Read, Show, Ord)

parseEvent :: Parser Event
parseEvent = do
  t <- parseTime
  _ <- P.char ' '
  a <- parseAction
  pure Event {eventTime = t, eventAction = a}

parseEventStream :: Parser [Event]
parseEventStream = P.many (parseEvent <* P.optional P.endOfLine)

data S = S
  { _sGuardSleepTrackers :: IntMap (IntMap Int)
  , _sCurrentGuardID :: Int
  , _sCurrentGuardAsleep :: Bool
  , _sCurrentMinute :: Int
  } deriving (Eq, Read, Show)

makeLenses ''S

trackerAddMinute :: Int -> Int -> IntMap (IntMap Int) -> IntMap (IntMap Int)
trackerAddMinute guard minute = IntMap.alter updateTracker guard
  where
    updateTracker Nothing = Just (IntMap.singleton minute 1)
    updateTracker (Just t) = Just (IntMap.alter updateMinute minute t)
    updateMinute Nothing = Just 1
    updateMinute (Just m) = Just (m + 1)

sStepMinute :: State S ()
sStepMinute = do
  guardAsleep <- use sCurrentGuardAsleep
  when guardAsleep $ do
    guardID <- use sCurrentGuardID
    time <- use sCurrentMinute
    sGuardSleepTrackers %= trackerAddMinute guardID time
  sCurrentMinute %= (\s -> (s + 1) `mod` 60)

sStepMinuteUntil :: Int -> State S ()
sStepMinuteUntil target = do
  t <- use sCurrentMinute
  when (t /= target) $ do
    sStepMinute
    sStepMinuteUntil target

handleEvent :: Event -> State S ()
handleEvent evt = do
  sStepMinuteUntil (timeMinute (eventTime evt))
  case eventAction evt of
    ActionWake -> sCurrentGuardAsleep .= False
    ActionSleep -> sCurrentGuardAsleep .= True
    ActionBeginShift guard -> sCurrentGuardID .= guard

main :: IO ()
main = do
  input <- readFile "input"
  let eventStream =
        case P.parse parseEventStream "" input of
          Left err -> error (show err)
          Right events -> sort events
      initState =
        S 
          { _sGuardSleepTrackers = mempty
          , _sCurrentGuardID = 0
          , _sCurrentGuardAsleep = False
          , _sCurrentMinute = 0
          }
      finalState = execState (mapM_ handleEvent eventStream) initState
  part1 finalState
  part2 finalState

trackerSleepiestMinute :: IntMap Int -> (Int, Int)
trackerSleepiestMinute tracker = last (sortOn snd (toList tracker))

part1 :: S -> IO ()
part1 s =
  putStrLn ("Part 1: " ++ show (sleepiestGuard * sleepiestMinute))
  where
    trackers = toList (_sGuardSleepTrackers s)
    minutesSlept (_, tracker) = sum (fmap snd (toList tracker))
    (sleepiestGuard, sleepiestTracker) = last (sortOn minutesSlept trackers)
    sleepiestMinute = fst (trackerSleepiestMinute sleepiestTracker)

part2 :: S -> IO ()
part2 s = putStrLn ("Part 2: " ++ show (sleepiestGuard * fst sleepiestMinute))
  where
    trackers = toList (_sGuardSleepTrackers s)
    (sleepiestGuard, sleepiestMinute) =
      last
        (sortOn
           (snd . snd)
           ((over (mapped . _2) trackerSleepiestMinute) trackers))
