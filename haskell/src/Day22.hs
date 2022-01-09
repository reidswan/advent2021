{-# LANGUAGE TupleSections #-}

module Day22 where

import Control.Monad.State
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Text.Parsec (ParseError, Parsec, char, newline, optional, parse, sepBy, string, try, (<|>))
import Util (parseSignedInteger, spaces)

type Parser a = Parsec String () a

type Range = (Integer, Integer)

data CommandType = On | Off deriving (Show, Eq)

data Cuboid = Cuboid
  { xRange :: Range,
    yRange :: Range,
    zRange :: Range
  }
  deriving (Show)

data Command = Command
  { typ :: CommandType,
    cuboid :: Cuboid
  }
  deriving (Show)

on_ :: Parser CommandType
on_ = string "on" >> return On

off_ :: Parser CommandType
off_ = string "off" >> return Off

commandType_ :: Parser CommandType
commandType_ = try on_ <|> off_

range_ :: Parser (Integer, Integer)
range_ = do
  start <- parseSignedInteger
  optional spaces
  string ".."
  end <- parseSignedInteger
  return (start, end)

command_ :: Parser Command
command_ = do
  typ <- commandType_
  spaces
  string "x="
  xRange <- range_
  char ','
  string "y="
  yRange <- range_
  char ','
  string "z="
  zRange <- range_
  return Command {typ, cuboid = Cuboid {xRange, yRange, zRange}}

commands_ :: Parser [Command]
commands_ = command_ `sepBy` newline

parseInput :: String -> Either ParseError [Command]
parseInput = parse commands_ ""

reduceRange :: Integer -> Integer -> Command -> Command
reduceRange minC maxC c@Command {cuboid = Cuboid {xRange, yRange, zRange}} = c {cuboid = Cuboid {xRange = xr', yRange = yr', zRange = zr'}}
  where
    reduceRange_ (a, b) = (max a minC, min b maxC)
    xr' = reduceRange_ xRange
    yr' = reduceRange_ yRange
    zr' = reduceRange_ zRange

reduceRanges :: Integer -> Integer -> [Command] -> [Command]
reduceRanges minC maxC = map (reduceRange minC maxC)

rangeEmpty :: Range -> Bool
rangeEmpty (a, b) = b < a

empty :: Cuboid -> Bool
empty Cuboid {xRange, yRange, zRange} = rangeEmpty xRange || rangeEmpty yRange || rangeEmpty zRange

rangeSize :: Range -> Integer
rangeSize (a, b) = if rangeEmpty (a, b) then 0 else b - a + 1

size :: Cuboid -> Integer
size Cuboid {xRange, yRange, zRange} = rangeSize xRange * rangeSize yRange * rangeSize zRange

intersection :: Cuboid -> Cuboid -> Maybe Cuboid
intersection Cuboid {xRange = xr1, yRange = yr1, zRange = zr1} Cuboid {xRange = xr2, yRange = yr2, zRange = zr2} = do
  xRange <- rangeIntersection xr1 xr2
  yRange <- rangeIntersection yr1 yr2
  zRange <- rangeIntersection zr1 zr2
  return $ Cuboid {xRange, yRange, zRange}

rangeIntersection :: Range -> Range -> Maybe Range
rangeIntersection (a1, b1) (a2, b2) =
  let a' = max a1 a2
      b' = min b1 b2
   in if rangeEmpty (a', b') then Nothing else Just (a', b')

flipMultiplier :: Integer -> Integer
flipMultiplier a
  | a >= 0 = -1
  | otherwise = 1

intersections :: Command -> State [(Integer, Cuboid)] [(Integer, Cuboid)]
intersections Command {cuboid, typ} = do
  cuboids <- get
  mCubs <-
    forM
      cuboids
      (\(mlt, cub) -> return $ fmap (flipMultiplier mlt,) (intersection cuboid cub))
  return $ catMaybes mCubs

runCommand :: Command -> State [(Integer, Cuboid)] ()
runCommand c@Command {cuboid, typ} = do
  ints <- intersections c
  existing <- get
  let newCuboids = existing ++ ints ++ [(1, cuboid) | typ == On]
  put newCuboids

runCommands :: [Command] -> State [(Integer, Cuboid)] ()
runCommands cs = forM_ cs runCommand

volume :: (Integer, Cuboid) -> Integer
volume (mlt, c) = mlt * size c

sumVolume :: [(Integer, Cuboid)] -> Integer
sumVolume = sum . map volume

part1 :: [Command] -> Integer
part1 cs = sumVolume cubs
  where
    cubs = execState (runCommands cs') []
    cs' = reduceRanges (-50) 50 cs

part2 :: [Command] -> Integer
part2 cs = sumVolume cubs
  where
    cubs = execState (runCommands cs) []

runDay22 :: String -> IO ()
runDay22 rawInput = case parseInput rawInput of
  Left err -> print err
  Right commands -> do
    putStrLn $ "Part 1: " ++ show (part1 commands)
    putStrLn $ "Part 2: " ++ show (part2 commands)