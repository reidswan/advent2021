-- heavily inspired by https://work.njae.me.uk/2021/12/21/advent-of-code-2021-day-19/

module Day19 where

import Control.Monad (guard)
import Data.Foldable (find, toList)
import Data.List (intersect, sort)
import qualified Data.Map as Map
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Set as Set
import Text.Parsec (ParseError, Parsec, char, endBy, many, many1, newline, notFollowedBy, optional, parse, sepBy, string, try, unexpected, (<|>))
import Util (parseInteger, parseSignedInteger)

type Parser a = Parsec String () a

type Coord = (Integer, Integer, Integer)

type Transformation = Coord -> Coord

data Scanner = Scanner
  { scannerId :: Integer,
    beacons :: Set.Set Coord,
    fingerprint :: [Integer]
  }
  deriving (Show)

data Path = Path
  { beaconLocs :: Set.Set Coord,
    scannerCenters :: Map.Map Integer Coord,
    pFingerprint :: [Integer]
  }
  deriving (Show)

dist :: Coord -> Coord -> Integer
dist (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

mkFingerprint :: [Coord] -> [Integer]
mkFingerprint beacons = sort [dist a b | a <- beacons, b <- beacons, a < b]

mkFingerprint' :: (Foldable f) => f Coord -> [Integer]
mkFingerprint' = mkFingerprint . toList

mkScanner :: Integer -> [Coord] -> Scanner
mkScanner id beacons = Scanner {scannerId = id, beacons = Set.fromList beacons, fingerprint = mkFingerprint beacons}

maybeMatch :: [Integer] -> [Integer] -> Bool
maybeMatch f1 f2 = countMatches f1 f2 0 >= div (12 * 11) 2
  where
    countMatches [] _ n = n
    countMatches _ [] n = n
    countMatches (a : as) (b : bs) n
      | a == b = countMatches as bs (n + 1)
      | a < b = countMatches as (b : bs) n
      | otherwise = countMatches (a : as) bs n

maybeMatch' :: Scanner -> Scanner -> Bool
maybeMatch' s1 s2 = maybeMatch (fingerprint s1) (fingerprint s2)

triple :: Parser (Integer, Integer, Integer)
triple = do
  ints <- parseSignedInteger `sepBy` char ','
  case ints of
    [a, b, c] -> return (a, b, c)
    _ -> unexpected "- wanted three ints separated by commas"

parseScanner :: Parser Scanner
parseScanner = do
  string "--- scanner "
  id <- parseInteger
  string " ---"
  newline
  beacons <- triple `endBy` newline
  optional newline
  return $ mkScanner id beacons

parseScanners :: Parser [Scanner]
parseScanners = parseScanner `sepBy` many newline

parseInput :: String -> Either ParseError [Scanner]
parseInput = parse parseScanners ""

rotX :: Transformation
rotX (x, y, z) = (x, - z, y)

rotY :: Transformation
rotY (x, y, z) = (z, y, - x)

rotZ :: Transformation
rotZ (x, y, z) = (- y, x, z)

possRots :: Transformation -> [Transformation]
possRots rot = [id, rot, rot . rot, rot . rot . rot]

rotations :: [Transformation]
rotations = [x . y . z | x <- possRots rotX, y <- possRots rotY, z <- possRots rotZ]

translateToMatch :: Coord -> Coord -> Transformation
translateToMatch (x, y, z) (x', y', z') (t, u, v) = (t + (x - x'), u + (y - y'), v + (z - z'))

findMap :: Foldable f => (a -> Maybe b) -> f a -> Maybe b
findMap pred =
  foldl
    ( \acc it -> case acc of
        Just x -> Just x
        Nothing -> pred it
    )
    Nothing

matchingTransformation :: Set.Set Coord -> Set.Set Coord -> Maybe Transformation
matchingTransformation beacons1 beacons2 =
  findMap
    ( \b1 ->
        findMap
          ( \b2 ->
              findMap
                ( \rotation ->
                    let translation = translateToMatch b1 (rotation b2)
                        transformation = translation . rotation
                        transformedB2 = Set.map transformation beacons2
                     in if length (Set.intersection beacons1 transformedB2) >= 12
                          then Just transformation
                          else Nothing
                )
                rotations
          )
          beacons2
    )
    beacons1

addScanner :: Path -> Transformation -> Scanner -> Path
addScanner Path {beaconLocs, scannerCenters} trans Scanner {beacons, scannerId} =
  Path
    { beaconLocs = bl',
      scannerCenters = Map.insert scannerId (trans (0, 0, 0)) scannerCenters,
      pFingerprint = mkFingerprint' bl'
    }
  where
    bl' = Set.union beaconLocs (Set.map trans beacons)

tryAdd :: Path -> Scanner -> Maybe Path
tryAdd path scanner = do
  t <- matchingTransformation (beaconLocs path) (beacons scanner)
  return $ addScanner path t scanner

scannerToPath :: Scanner -> Path
scannerToPath Scanner {beacons, scannerId, fingerprint} =
  Path
    { beaconLocs = beacons,
      scannerCenters = Map.singleton scannerId (0, 0, 0),
      pFingerprint = fingerprint
    }

matchAndAddAll :: [Scanner] -> Path
matchAndAddAll [] = undefined
matchAndAddAll (head : rest) = go (scannerToPath head) rest [] (length rest)
  where
    go p [] [] _ = p
    go p [] stack startLen
      | length stack == startLen = error "unable to make any progress"
      | otherwise = go p stack [] (length stack)
    go p (head : rest) stack l
      | maybeMatch (pFingerprint p) (fingerprint head) = case tryAdd p head of
        Just p' -> go p' rest stack l
        Nothing -> go p rest (head : stack) l
      | otherwise = go p rest (head : stack) l

part1 :: Path -> Int
part1 = length . beaconLocs

manhattanDist :: Num a => (a, a, a) -> (a, a, a) -> a
manhattanDist (x, y, z) (x', y', z') = abs (x - x') + abs (y - y') + abs (z - z')

part2 :: Path -> Integer
part2 path =
  let pos = toList $ Map.elems $ scannerCenters path
      dists = [manhattanDist x y | x <- pos, y <- pos, x < y]
   in maximum dists

runDay19 :: [Char] -> IO ()
runDay19 rawInput = do
  let input = parseInput rawInput
  case input of
    Left err -> print err
    Right scanners -> do
      let path = matchAndAddAll scanners
      putStrLn $ "Part 1: " ++ show (part1 path)
      putStrLn $ "Part 2: " ++ show (part2 path)
