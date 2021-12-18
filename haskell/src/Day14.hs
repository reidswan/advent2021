{-# LANGUAGE NamedFieldPuns #-}

module Day14 where

import Data.Foldable (maximumBy, minimumBy)
import Data.List (group, sort)
import Data.Map.Strict ((!), (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Text.Parsec
  ( ParseError,
    Parsec,
    anyChar,
    many,
    many1,
    newline,
    parse,
    sepBy1,
    string,
    upper,
  )
import Util (countItems, parseInteger, spaces)

type Parser a = Parsec String () a

data PairInsertion = Insertion Char Char Char

parsePairInsertion :: Parser PairInsertion
parsePairInsertion = do
  t1 <- upper
  t2 <- upper
  string " -> "
  Insertion t1 t2 <$> upper

data PolymerScript = Script
  { pairCounts :: Map.Map (Char, Char) Integer,
    insertions :: Map.Map (Char, Char) Char,
    start :: Char,
    end :: Char
  }
  deriving (Show)

makePairCounts :: String -> Map.Map (Char, Char) Integer
makePairCounts s = countItems $ zip s $ tail s

parsePolymerScript :: Parser PolymerScript
parsePolymerScript = do
  template <- many1 upper
  many1 newline
  insertionsL <- sepBy1 parsePairInsertion newline
  let insertions = Map.fromList $ map (\(Insertion a b c) -> ((a, b), c)) insertionsL
      pairCounts = makePairCounts template
      start = head template
      end = last template
  return $ Script {pairCounts, insertions, start, end}

addOrSet :: Integer -> Maybe Integer -> Maybe Integer
addOrSet i (Just a) = Just $ i + a
addOrSet i _ = Just i

maybeExpandPolymer :: PolymerScript -> (Char, Char) -> Integer -> PolymerScript
maybeExpandPolymer s@Script {pairCounts, insertions} (a, b) pairCount
  | Map.member (a, b) insertions =
    let newElem = insertions ! (a, b)
        -- there is now an (a, newElem) and a (newElem, b) for every previous (a, b)
        pc = Map.alter (addOrSet pairCount) (a, newElem) pairCounts
        pc' = Map.alter (addOrSet pairCount) (newElem, b) pc
     in s {pairCounts = pc'}
  | otherwise = s

expandPolymers :: PolymerScript -> PolymerScript
expandPolymers ps = Map.foldlWithKey maybeExpandPolymer (ps {pairCounts = Map.empty}) (pairCounts ps)

expandPolymersN :: Integer -> PolymerScript -> PolymerScript
expandPolymersN 0 v = v
expandPolymersN n v = expandPolymersN (n - 1) (expandPolymers v)

parseInput :: String -> Either ParseError PolymerScript
parseInput = parse parsePolymerScript ""

countElems :: PolymerScript -> Map.Map Char Integer
countElems Script {start, end, pairCounts} =
  let rawCounts =
        Map.foldlWithKey
          ( \m (a, b) v ->
              let m1 = Map.alter (addOrSet v) a m
                  m2 = Map.alter (addOrSet v) b m1
               in m2
          )
          Map.empty
          pairCounts
      counts = Map.map (`div` 2) rawCounts
      -- the start and end values are the only elements not double counted in the sum, so add 1 back to each
      counts' = Map.update (\a -> Just $ a + 1) start counts
      counts'' =
        if start /= end
          then Map.update (\a -> Just $ a + 1) end counts'
          else counts'
   in counts''

mostByCount :: Ord v => Map.Map k v -> Maybe (k, v)
mostByCount =
  Map.foldlWithKey
    ( \lrg k v -> case lrg of
        Nothing -> Just (k, v)
        Just (k', v') -> if v > v' then Just (k, v) else Just (k', v')
    )
    Nothing

leastByCount :: Ord v => Map.Map k v -> Maybe (k, v)
leastByCount =
  Map.foldlWithKey
    ( \sml k v -> case sml of
        Nothing -> Just (k, v)
        Just (k', v') -> if v < v' then Just (k, v) else Just (k', v')
    )
    Nothing

runPart :: Integer -> PolymerScript -> Integer
runPart i ps =
  let expanded = expandPolymersN i ps
      counts = countElems expanded
      Just (kl, largest) = mostByCount counts
      Just (ks, smallest) = leastByCount counts
   in largest - smallest

part1 :: PolymerScript -> Integer
part1 = runPart 10

part2 :: PolymerScript -> Integer
part2 = runPart 40

runDay14 :: String -> IO ()
runDay14 rawInput = do
  let input = parseInput rawInput
  case input of
    Left err -> print err
    Right ps -> do
      putStrLn $ "Part 1: " ++ show (part1 ps)
      putStrLn $ "Part 2: " ++ show (part2 ps)
