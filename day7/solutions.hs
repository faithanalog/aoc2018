{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

-- dependencies: lens, containers, mtl, text

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text

data Worker = Worker
  { _wTask :: Char
  , _wTime :: Int
  } deriving (Eq, Read, Show)

data SystemState = SystemState
  { _sWorkersIdle :: Int
  , _sWorkersBusy :: [Worker]
  , _sTasksCompleted :: Set Char
  , _sTasksRemaining :: Set Char
  , _sDependencies :: Char -> Set Char
  , _sWorkLog :: [Char]
  , _sTime :: Int
  }

makeLenses ''Worker
makeLenses ''SystemState
type System a = State SystemState a

newSystemState :: Int -> (Char -> Set Char) -> SystemState
newSystemState numWorkers dependencies =
  SystemState
    { _sWorkersIdle = numWorkers
    , _sWorkersBusy = mempty
    , _sTasksCompleted = mempty
    , _sTasksRemaining = Set.fromList ['A' .. 'Z']
    , _sDependencies = dependencies
    , _sWorkLog = mempty
    , _sTime = 0
    }

parseDependency :: Text -> (Char, Char)
parseDependency ln = (Text.head (fields !! 7), Text.head (fields !! 1))
  where
    fields = Text.split (== ' ') ln

collectDependencies :: [(Char, Char)] -> (Char -> Set Char)
collectDependencies dependencyEdges =
  (\c -> fromMaybe Set.empty (Map.lookup c final))
  where
    insert (step, dep) = Map.adjust (Set.insert dep) step
    initial = Map.fromList [(step, mempty) | step <- ['A' .. 'Z']]
    final = foldr (.) id (fmap insert dependencyEdges) initial

assignNextTask :: System Bool
assignNextTask = do
  idle <- use sWorkersIdle
  task <- nextTask
  if idle <= 0 || isNothing task
    then pure False
    else do
      assignTask (fromJust task)
      pure True
  where
    nextTask = do
      remaining <- use sTasksRemaining
      dependencies <- use sDependencies
      completed <- use sTasksCompleted
      let canRun c = dependencies c `Set.isSubsetOf` completed
      pure (find canRun remaining)
    taskTime task = 61 + (fromEnum task - fromEnum 'A')
    assignTask task = do
      sTasksRemaining %= Set.delete task
      sWorkersIdle -= 1
      sWorkersBusy %= (Worker task (taskTime task) :)

assignAllRunnableTasks :: System ()
assignAllRunnableTasks = do
  assigned <- assignNextTask
  when assigned assignAllRunnableTasks

advanceTime :: System ()
advanceTime = do
  timeStep <- uses sWorkersBusy (minimum1Of (traverse . wTime))
  sWorkersBusy . traverse . wTime -= timeStep
  sTime += timeStep

freeDoneWorkers :: System ()
freeDoneWorkers = do
  workers <- use sWorkersBusy
  let isDone w = _wTime w <= 0
      done = filter isDone workers
      notDone = filter (not . isDone) workers
  forM_ done $ \w -> do
    sWorkLog %= (_wTask w :)
    sTasksCompleted %= Set.insert (_wTask w)
    sWorkersIdle += 1
  sWorkersBusy .= notDone

isSystemDone :: System Bool
isSystemDone = do
  allTasksSubmitted <- uses sTasksRemaining null
  allWorkersDone <- uses sWorkersBusy null
  pure (allTasksSubmitted && allWorkersDone)

mainSystemLoop :: System ()
mainSystemLoop = do
  assignAllRunnableTasks
  advanceTime
  freeDoneWorkers
  done <- isSystemDone
  unless done mainSystemLoop

runWithWorkers :: Int -> (Char -> Set Char) -> (String, Int)
runWithWorkers numWorkers deps = (reverse (_sWorkLog sys), _sTime sys)
  where
    sys = execState mainSystemLoop (newSystemState numWorkers deps)

main :: IO ()
main = do
  input <- Text.readFile "input"
  let dependencies =
        collectDependencies (fmap parseDependency (Text.lines input))
  putStr "Part 1: "
  putStrLn (fst (runWithWorkers 1 dependencies))
  putStr "Part 2: "
  print (snd (runWithWorkers 5 dependencies))
  
