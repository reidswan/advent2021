module Day1 where

import Data.List (group)
import Data.String (IsString (fromString))

part1 :: [Integer] -> Integer
part1 (head : tail) =
  let go cnt _ [] = cnt
      go cnt prev (curr : rest)
        | curr > prev = go (cnt + 1) curr rest
        | otherwise = go cnt curr rest
   in go 0 head tail
part1 _ = error "empty list"

part2 :: [Integer] -> Integer
part2 (fst : snd : thd : tail) =
  let go cnt _ [] = cnt
      go cnt (f, s, t) (curr : rest)
        | curr > f = go (cnt + 1) (s, t, curr) rest
        | otherwise = go cnt (s, t, curr) rest
   in go 0 (fst, snd, thd) tail
part2 _ = error "empty list"

parseInput :: String -> [Integer]
parseInput = map read . filter (/= "") . lines

runDay1 rawInput = do
  let input = parseInput rawInput
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
