module Main where

-- dependencies: vector

import Control.Monad.State.Lazy
import qualified Data.Vector.Mutable as MVec
import qualified Data.Vector as Vec
import Data.IORef
import System.Exit

data V2 =
  V2 Int
     Int

width :: Int
width = 80

height :: Int
height = 10

-- A fun example of writing C in Haskell

run :: [(V2, V2)] -> IO ()
run initialStuff = do
  xs <- MVec.new (length initialStuff)
  forM_ (zip [0..] initialStuff) $ \(i, x) -> MVec.write xs i x
  minXR <- newIORef maxBound
  minYR <- newIORef maxBound
  maxXR <- newIORef minBound
  maxYR <- newIORef minBound
  seconds <- newIORef (0 :: Int)
  forever $ do
    modifyIORef' seconds (+1)
    writeIORef minXR maxBound
    writeIORef minYR maxBound
    writeIORef maxXR minBound
    writeIORef maxYR minBound
    forM_ [0 .. MVec.length xs - 1] $ \i -> do
      (V2 px py, V2 vx vy) <- MVec.read xs i
      modifyIORef' minXR (min (px + vx))
      modifyIORef' minYR (min (py + vy))
      modifyIORef' maxXR (max (px + vx))
      modifyIORef' maxYR (max (py + vy))
      MVec.write xs i (V2 (px + vx) (py + vy), V2 vx vy)
    minX <- readIORef minXR
    minY <- readIORef minYR
    maxX <- readIORef maxXR
    maxY <- readIORef maxYR
    when ((maxX - minX) < width && (maxY - minY) < height) $ do
      outputBuffer <- MVec.new (width * height)
      forM_ [0 .. (width * height) - 1] $ \i -> MVec.write outputBuffer i ' '
      forM_ [0 .. MVec.length xs - 1] $ \i -> do
        (V2 px py, V2 vx vy) <- MVec.read xs i
        let dx = px - minX
        let dy = py - minY
        MVec.write outputBuffer ((px - minX) + (py - minY) * width) '#'
      putStrLn "Part 1: "
      forM_ [0 .. height - 1] $ \y -> do
        forM_ [0 .. width - 1] $ \x -> do
          c <- MVec.read outputBuffer (x + y * width)
          putChar c
        putChar '\n'
      putStr "Part 2: "
      readIORef seconds >>= print
      exitSuccess
  
parse :: String -> (V2, V2)
parse = evalState $ do
    modify (drop (length "position=<"))
    px <- state (splitAt (length "-XXXXX"))
    modify (drop (length ", "))
    py <- state (splitAt (length "-XXXXX"))
    modify (drop (length "> velocity=<"))
    vx <- state (splitAt (length "-X"))
    modify (drop (length ", "))
    vy <- state (splitAt (length "-X"))
    pure (V2 (read px) (read py), V2 (read vx) (read vy))

  

main :: IO ()
main = do
  input <- readFile "input"
  let vecs = fmap parse (lines input)
  run vecs
