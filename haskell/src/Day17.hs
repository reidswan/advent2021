{-# LANGUAGE NamedFieldPuns #-}

module Day17 where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Text.Parsec (ParseError, Parsec, char, count, eof, many, many1, oneOf, parse, string, unexpected)
import Util (inBounds, parseSignedInteger, spaces)

type Parser a = Parsec String () a

data Target = Target
  { xMin :: Integer,
    xMax :: Integer,
    yMin :: Integer,
    yMax :: Integer
  }
  deriving (Show)

{-
  y values:
  v[n] = v[0] - n
  y[n]  = y[n-1] + v[n-1]
        = y[n-1] + v[0] - (n-1)
        = (y[n-2] + v[n-2]) + v[0] - (n-1)
        = (y[n-2] + v[0] - (n-2)) + v[0] - (n-1)
        = y[n-2] + 2*v[0] - (n-2) - (n-1)
        ...
        = y[n-n] + n*v[0] - sum(i=1..n-1)(i)

  y[k]    = k * v[0] - (k ^ 2 - k)/2
  2*y[k]  = 2 * k * v[0] - k^2 + k
  0       = k ^ 2 - (2 * v[0] + 1) * k + 2 * y[k]

  => k[y, v] = (2 * v + 1 - sqrt((1 + 2 * v)^2 - 8))/2

  if there is no integer in range [k[yMax, v] .. k[yMin, v]] then eliminate v from consideration
  else yApex = y[v[0]]
-}

determineStep :: Integer -> Integer -> Maybe Float
determineStep y v =
  let b = - (1 + 2 * v)
      disc = b ^ 2 - 8 * y
   in if disc < 0 then Nothing else Just $ (fromIntegral (- b) + sqrt (fromIntegral disc)) / 2

hitsTargetY :: Target -> Integer -> Bool
hitsTargetY Target {yMax, yMin} v0 = fromMaybe False $ do
  stepMin <- determineStep yMin v0
  stepMax <- determineStep (yMax + 1) v0
  return $ floor stepMax < floor stepMin

determineApex :: Integer -> Integer
determineApex v0 = let v = v0 + 1 in div (v ^ 2 - v) 2

validVelocity :: Target -> (Integer, Integer) -> Bool
validVelocity Target {xMin, xMax, yMin, yMax} = go (0, 0)
  where
    go (x, y) (vx, vy)
      | x `inBounds` (xMin, xMax) && y `inBounds` (yMin, yMax) = True
      | vx <= 0 && x < xMin = False
      | x > xMax || y < yMin = False
      | otherwise = go (x + vx, y + vy) (if vx == 0 then 0 else vx - 1, vy - 1)

parseTarget :: Parser Target
parseTarget = do
  string "target area: x="
  x1 <- parseSignedInteger
  string ".."
  x2 <- parseSignedInteger
  char ','
  spaces
  string "y="
  y1 <- parseSignedInteger
  string ".."
  y2 <- parseSignedInteger
  let xMin = min x1 x2
      xMax = max x1 x2
      yMin = min y1 y2
      yMax = max y1 y2
  return $ Target {xMin, xMax, yMin, yMax}

parseInput :: String -> Either ParseError Target
parseInput = parse parseTarget ""

part1 :: Target -> Integer
part1 t = maximum $ map determineApex $ filter (hitsTargetY t) [yMin t .. 1000]

part2 :: Target -> Int
part2 t =
  let validYVels = filter (hitsTargetY t) [yMin t .. 1000]
      validXYVels = filter (validVelocity t) [(vx, vy) | vx <- [0 .. xMax t + 1], vy <- validYVels]
   in length validXYVels

runDay17 :: String -> IO ()
runDay17 rawInput = do
  let input = parseInput rawInput
  case input of
    Left err -> print err
    Right t -> do
      putStrLn $ "Part 1: " ++ show (part1 t)
      putStrLn $ "Part 2: " ++ show (part2 t)
