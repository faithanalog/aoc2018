{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE RankNTypes #-}
module Main where

-- dependencies: lens, containers, parsec

import Control.Lens
import Data.Bits
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Text.Parsec as P

data Op
  = OpAddr
  | OpAddi
  | OpMulr
  | OpMuli
  | OpBanr
  | OpBani
  | OpBorr
  | OpBori
  | OpSetr
  | OpSeti
  | OpGtir
  | OpGtri
  | OpGtrr
  | OpEqir
  | OpEqri
  | OpEqrr
  deriving (Eq, Read, Show, Ord, Enum, Bounded)

data Instr =
  Instr !Op
        !Int
        !Int
        !Int
  deriving (Eq, Read, Show)

data InstrRaw =
  InstrRaw !Int
           !Int
           !Int
           !Int
  deriving (Eq, Read, Show)

data Machine = Machine
  { mReg0 :: !Int
  , mReg1 :: !Int
  , mReg2 :: !Int
  , mReg3 :: !Int
  } deriving (Eq, Read, Show)

mReg :: Int -> Lens' Machine Int
mReg 0 f m = fmap (\val -> m {mReg0 = val}) (f (mReg0 m))
mReg 1 f m = fmap (\val -> m {mReg1 = val}) (f (mReg1 m))
mReg 2 f m = fmap (\val -> m {mReg2 = val}) (f (mReg2 m))
mReg 3 f m = fmap (\val -> m {mReg3 = val}) (f (mReg3 m))
mReg n _ _ = error ("Main.mReg: undefined register " ++ show n)

runInstr :: Instr -> Machine -> Machine
runInstr (Instr OpAddr a b c) m = set (mReg c) (view (mReg a) m + view (mReg b) m) m
runInstr (Instr OpAddi a b c) m = set (mReg c) (view (mReg a) m + b) m
runInstr (Instr OpMulr a b c) m = set (mReg c) (view (mReg a) m * view (mReg b) m) m
runInstr (Instr OpMuli a b c) m = set (mReg c) (view (mReg a) m * b) m
runInstr (Instr OpBanr a b c) m = set (mReg c) (view (mReg a) m .&. view (mReg b) m) m
runInstr (Instr OpBani a b c) m = set (mReg c) (view (mReg a) m .&. b) m
runInstr (Instr OpBorr a b c) m = set (mReg c) (view (mReg a) m .|. view (mReg b) m) m
runInstr (Instr OpBori a b c) m = set (mReg c) (view (mReg a) m .|. b) m
runInstr (Instr OpSetr a b c) m = set (mReg c) (view (mReg a) m) m
runInstr (Instr OpSeti a b c) m = set (mReg c) a m
runInstr (Instr OpGtir a b c) m =
  if a > view (mReg b) m
    then set (mReg c) 1 m
    else set (mReg c) 0 m
runInstr (Instr OpGtri a b c) m =
  if view (mReg a) m > b
    then set (mReg c) 1 m
    else set (mReg c) 0 m
runInstr (Instr OpGtrr a b c) m =
  if view (mReg a) m > view (mReg b) m
    then set (mReg c) 1 m
    else set (mReg c) 0 m
runInstr (Instr OpEqir a b c) m =
  if a == view (mReg b) m
    then set (mReg c) 1 m
    else set (mReg c) 0 m
runInstr (Instr OpEqri a b c) m =
  if view (mReg a) m == b
    then set (mReg c) 1 m
    else set (mReg c) 0 m
runInstr (Instr OpEqrr a b c) m =
  if view (mReg a) m == view (mReg b) m
    then set (mReg c) 1 m
    else set (mReg c) 0 m

data Sample = Sample
  { sBefore :: Machine
  , sInstr :: InstrRaw
  , sAfter :: Machine
  } deriving (Eq, Read, Show)

data Input = Input
  { iSamples :: [Sample]
  , iProg :: [InstrRaw]
  } deriving (Eq, Read, Show)

parseInput :: String -> Input
parseInput str =
  let num = do
        digits <- P.many P.digit
        pure (read digits)
      quad ctor p sep = do
        [a, b, c, d] <- P.sepBy1 p sep
        pure (ctor a b c d)
      machine = quad Machine num (P.string ", ")
      instrRaw = quad InstrRaw num (P.char ' ')
      sample = do
        _ <- P.string "Before: ["
        before <- machine
        _ <- P.string "]\n"
        instr <- instrRaw
        _ <- P.string "\nAfter:  ["
        after <- machine
        _ <- P.string "]\n"
        pure (Sample before instr after)
      input = do
        samples <- P.sepEndBy1 sample (P.char '\n')
        _ <- P.string "\n\n"
        instrs <- P.sepEndBy1 instrRaw (P.char '\n')
        pure (Input samples instrs)
   in case P.parse input "Input" str of
        Left err -> error ("Main.parseInput: " ++ show err)
        Right x -> x

type Constraints = Map Int (Set Op)

potentialOps :: Sample -> Set Op
potentialOps s =
  let behavesLike (Sample before (InstrRaw _ a b c) after) op =
        runInstr (Instr op a b c) before == after
   in Set.fromList (filter (behavesLike s) (enumFromTo minBound maxBound))

initialConstraints :: Constraints
initialConstraints =
  Map.fromList
    [(i, Set.fromList (enumFromTo minBound maxBound)) | i <- [0 .. 15]]

updateConstraints :: Sample -> Constraints -> Constraints
updateConstraints sample constraints =
  let InstrRaw opcode _ _ _ = sInstr sample
      currentPotentialOps = constraints Map.! opcode
      newPotentialOps =
        Set.intersection currentPotentialOps (potentialOps sample)
      updatedConstraints =
        if length newPotentialOps == 1
          then Map.insert
                 opcode
                 newPotentialOps
                 (fmap (\ops -> Set.difference ops newPotentialOps) constraints)
          else Map.insert opcode newPotentialOps constraints
   in updatedConstraints

solveConstraints :: [Sample] -> Constraints
solveConstraints samples =
  appEndo (foldMap (Endo . updateConstraints) samples) initialConstraints

opcodeMap :: Constraints -> Int -> Op
opcodeMap constraints = (fmap (Set.elemAt 0) constraints Map.!)

decode :: Constraints -> InstrRaw -> Instr
decode constraints =
  let dec = opcodeMap constraints
   in \(InstrRaw opcode a b c) -> Instr (dec opcode) a b c

decodeInput :: Input -> [Instr]
decodeInput input =
  let constraints = solveConstraints (iSamples input)
   in fmap (decode constraints) (iProg input)

executeProgram :: [Instr] -> Machine
executeProgram = foldl' (\m i -> runInstr i m) (Machine 0 0 0 0)

part1 :: Input -> Int
part1 input =
  length (filter (\s -> length (potentialOps s) >= 3) (iSamples input))

part2 :: Input -> Int
part2 input = mReg0 (executeProgram (decodeInput input))

main :: IO () 
main = do
  input <- fmap parseInput (readFile "input")
  putStr "Part 1: "
  print (part1 input)
  putStr "Part 2: "
  print (part2 input)
