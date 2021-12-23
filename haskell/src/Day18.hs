module Day18 where

import Data.Either (fromRight)
import Debug.Trace (traceShowId)
import Text.Parsec (ParseError, Parsec, char, eof, many1, newline, optional, parse, sepBy, try, (<|>))
import Util (parseInteger)

type Parser a = Parsec String () a

data SnailNumberNode = Node {value :: Integer, depth :: Integer} deriving (Show)

type SnailNumber = [SnailNumberNode]

regularNumber :: Integer -> Parser SnailNumberNode
regularNumber depth = do
  i <- parseInteger
  optional $ char ','
  return $ Node {value = i, depth = depth}

snailNumber :: Integer -> Parser SnailNumber
snailNumber depth = do
  try
    ( do
        sn <- regularNumber depth
        rest <- snailNumber depth
        return $ sn : rest
    )
    <|> try (char '[' >> snailNumber (depth + 1))
    <|> ( do
            char ']'
            if depth <= 1
              then return []
              else do
                optional $ char ','
                snailNumber (depth - 1)
        )

snailNumbers :: Parser [SnailNumber]
snailNumbers = snailNumber 0 `sepBy` newline

parseInput :: String -> Either ParseError [SnailNumber]
parseInput = parse snailNumbers ""

canExplode :: SnailNumber -> Bool
canExplode = any (\a -> depth a > 4)

canSplit :: SnailNumber -> Bool
canSplit = any (\a -> value a >= 10)

explode :: SnailNumber -> SnailNumber
explode sn = go sn []
  where
    go (m : n : rest) stack
      | depth m > 4 =
        let stack' = addToStart (value m) stack
            rest' = addToStart (value n) rest
         in reverse stack' ++ (Node {depth = depth m - 1, value = 0} : rest')
      | otherwise = go (n : rest) (m : stack)
    go [a] stack = reverse (a : stack)
    go [] stack = reverse stack

    addToStart _ [] = []
    addToStart v (Node {depth, value} : rest) = Node {depth, value = value + v} : rest

split :: SnailNumber -> SnailNumber
split sn = go sn []
  where
    go [] stack = reverse stack
    go (n : rest) stack
      | value n >= 10 =
        let (l, r) = splitValue (value n)
            depth' = depth n + 1
         in reverse stack ++ (Node {depth = depth', value = l} : Node {depth = depth', value = r} : rest)
      | otherwise = go rest (n : stack)

    splitValue v =
      let v' = div v 2
       in if odd v then (v', v' + 1) else (v', v')

reduce :: SnailNumber -> SnailNumber
reduce sn
  | canExplode sn = reduce (explode sn)
  | canSplit sn = reduce (split sn)
  | otherwise = sn

addSN :: SnailNumber -> SnailNumber -> SnailNumber
addSN sn1 sn2 = reduce (map incrDepth sn1 ++ map incrDepth sn2)
  where
    incrDepth Node {depth, value} = Node {depth = depth + 1, value}

recursiveMagnitudeSum :: SnailNumber -> SnailNumber
recursiveMagnitudeSum sn = go sn [] maxDepth
  where
    maxDepth = maximum $ map depth sn
    go t _ 0 = t
    go [] stack n = go (reverse stack) [] (n - 1)
    go [a] stack n = go (reverse (a : stack)) [] (n - 1)
    go (m : n : rest) stack d
      | depth m == d = go rest (Node {value = 3 * value m + 2 * value n, depth = d - 1} : stack) d
      | otherwise = go (n : rest) (m : stack) d

magnitude :: SnailNumber -> Integer
magnitude sn = value $ head $ recursiveMagnitudeSum sn

part1 :: [SnailNumber] -> Integer
part1 = magnitude . foldl1 addSN

part2 :: [SnailNumber] -> Integer
part2 sns =
  let enum = zip sns [1 ..]
      pairs = [(x, y) | (x, i) <- enum, (y, j) <- enum, i /= j]
      mags = map (magnitude . uncurry addSN) pairs
   in maximum mags

runDay18 rawInput = do
  let input = parseInput rawInput
  case input of
    Left err -> print err
    Right sns -> do
      putStrLn $ "Part 1: " ++ show (part1 sns)
      putStrLn $ "Part 2: " ++ show (part2 sns)
