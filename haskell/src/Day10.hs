module Day10 where

import Data.List (sort)
import Data.Maybe (catMaybes, mapMaybe)
import Debug.Trace (trace)
import Util (head')

isOpener c = c `elem` "([{<"

closer '(' = ')'
closer '[' = ']'
closer '{' = '}'
closer '<' = '>'
closer c = error $ "not a valid bracket: '" ++ c : "'"

-- do o and c form a valid chunk pair
matches o c = closer o == c

-- the score value of a corrupt char
scorep1 ')' = 3
scorep1 ']' = 57
scorep1 '}' = 1197
scorep1 '>' = 25137
scorep1 c = error $ "not a valid bracket: '" ++ c : "'"

scorep2 ')' = 1
scorep2 ']' = 2
scorep2 '}' = 3
scorep2 '>' = 4
scorep2 c = error $ "not a valid bracket: '" ++ c : "'"

completionScore line = do
  completions <- close line []
  return $ foldl (\tot c -> 5 * tot + scorep2 c) 0 completions

part1 = sum . map corruptionScore

-- get the first char that causes a mismatch in the line
corruption [] _ = Nothing
corruption (head : rest) stack
  | isOpener head = corruption rest (head : stack)
  | otherwise = case head' stack of
    Nothing -> Just head
    Just o | matches o head -> corruption rest $ tail stack
    Just o -> Just head

part2 lines =
  let scores = mapMaybe completionScore lines
      sorted = sort scores
   in sorted !! (length sorted `div` 2)

-- get the closing bracket for each open bracket in the given stack
completions :: [Char] -> [Char]
completions = map closer

close :: [Char] -> [Char] -> Maybe [Char]
close [] stack = Just $ completions stack
close (c : cs) stack
  | isOpener c = close cs (c : stack)
  | otherwise = case head' stack of
    Nothing -> Nothing
    Just o | matches o c -> close cs (tail stack)
    Just o -> Nothing

corruptionScore line = maybe 0 scorep1 $ corruption line []

parseInput = lines

runDay10 rawInput = do
  let input = parseInput rawInput
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
