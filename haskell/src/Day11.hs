{-# LANGUAGE FlexibleContexts #-}
module Day11 where
import Util (parseInteger, unsafeFromDecDigit)
import Data.Array.Unboxed (UArray, array, (!))
import Data.Array.ST (runSTUArray, writeArray, STUArray, newArray, readArray, thaw, MArray)
import Control.Monad (forM_, when, foldM, forM)
import Data.Array.MArray (mapArray)
import Control.Monad.ST
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import Debug.Trace (trace)
import Data.Maybe (fromJust, catMaybes)

type HashGrid s  = C.HashTable s (Int, Int) Int
type AssocList = [((Int, Int), Int)]

parseInput :: String -> AssocList
parseInput s =
    let
        ls = lines s
        nums = map (map unsafeFromDecDigit) ls
        assocFn (x, row) = zipWith (\y e -> ((x, y), e)) [0..] row
    in
        concatMap assocFn $ zip [0..] nums

initHashTable :: AssocList -> ST s (HashGrid s)
initHashTable = H.fromList

adjacentIndices (x, y) = [
    (x + dx, y + dy) |
        dx <- [-1..1],
        dy <- [-1..1],
        dx /= 0 || dy /= 0]

showHT :: HashGrid s -> ST s String 
showHT h = do
    foldM (\ss i -> do
        line <- foldM (\s j -> do 
            n <- H.lookup h (i, j)
            return $ s ++ show (fromJust n)) "" [0..9]
        return $ ss ++ '\n':line) "" [0..9]

incrementAll :: HashGrid s -> ST s [(Int, Int)]
incrementAll h = do
    H.foldM (\l (k, v) -> do
        H.insert h k (v + 1)
        return $ if v == 9 then k:l else l) [] h

incrementAdjacent :: HashGrid s -> [(Int, Int)] -> ST s ()
incrementAdjacent _ [] = return ()
incrementAdjacent h (head:rest) = do
    flashing <- forM (adjacentIndices head) (\k -> do
        v_ <- H.lookup h k
        case v_ of 
            Just v -> do 
                H.insert h k (v+1)
                return $ if v == 9 then Just k else Nothing
            Nothing -> return Nothing)
    incrementAdjacent h (rest ++ catMaybes flashing)

countAndResetFlashes :: HashGrid s -> ST s Int
countAndResetFlashes h = H.foldM (\n (k, v) -> 
    if v > 9 
        then do
            H.insert h k 0 
            return $ n + 1
        else return n) 0 h

step :: HashGrid s -> ST s Int
step h = do
    flashing <- incrementAll h
    incrementAdjacent h flashing
    countAndResetFlashes h

runP1 :: Int -> HashGrid s -> Int -> ST s Int
runP1 0 _ total = return total
runP1 n h total = do 
    flashes <- step h
    runP1 (n-1) h (flashes + total)

runP2 :: Int -> HashGrid s -> ST s Int
runP2 n h = do
    flashes <- step h
    if flashes == 100 then return n else runP2 (n + 1) h

part1 :: AssocList -> Int
part1 input = runST $ do
    ht <- initHashTable input
    runP1 100 ht 0

part2 :: AssocList -> Int
part2 input = runST $ do
    ht <- initHashTable input
    runP2 1 ht

runDay11 :: String -> IO ()
runDay11 s = do
    let input = parseInput s
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)