module Day15 where

import Control.Monad (forM_, when)
import Data.Array ((!))
import qualified Data.Array as Array
import Data.Array.ST (MArray (newArray), STArray, freeze, readArray, runSTArray, writeArray)
import Data.List (sortBy)
import qualified Data.Set as Set
import Util (adjacent, readGrid)

-- lazy dynamic programming a la https://jelv.is/blog/Lazy-Dynamic-Programming/
-- only works for (my) part 1 because it assumes only right or down moves
-- I ended up redoing this in rust so go check that out instead
minRiskScore :: Array.Array (Int, Int) Int -> Int
minRiskScore grid = go (0, 0)
  where
    (_, (ht, wid)) = Array.bounds grid
    go (i, j)
      | (i, j) == (ht, wid) = grid ! (i, j)
      | i == ht = grid ! (i, j) + dynamicStore ! (i, j + 1)
      | j == wid = grid ! (i, j) + dynamicStore ! (i + 1, j)
      | otherwise =
        let curr = grid ! (i, j)
            right = dynamicStore ! (i, j + 1)
            down = dynamicStore ! (i + 1, j)
            next = min right down
         in curr + next
    dynamicStore = Array.listArray ((0, 0), (ht, wid)) [go (i, j) | i <- [0 .. ht], j <- [0 .. wid]]

part1 :: Array.Array (Int, Int) Int -> Int
part1 grid = minRiskScore grid - grid ! (0, 0)

computeNewVal :: Integral a => a -> a -> a -> a
computeNewVal v i j = ((v - 1 + i + j) `mod` 9) + 1

expandGrid :: Array.Array (Int, Int) Int -> Array.Array (Int, Int) Int
expandGrid grid =
  let (_, (ht, wd)) = Array.bounds grid
      expandAssocs ((x, y), v) = [((x + ht * i, y + wd * j), computeNewVal v i j) | i <- [0 .. 4], j <- [0 .. 4]]
      assocs = concatMap expandAssocs $ Array.assocs grid
      newHt = 5 * ht
      newWd = 5 * wd
   in Array.array ((0, 0), (newHt, newWd)) assocs

runDay15 :: String -> IO ()
runDay15 rawInput = do
  let input = readGrid rawInput
  putStrLn $ "Part 1: " ++ show (part1 input)