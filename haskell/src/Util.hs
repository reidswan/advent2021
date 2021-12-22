module Util where

import qualified Data.Array as Array
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (find)
import Data.List (elemIndex, findIndex)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Parsec (Parsec, digit, many1, option, space)
import Text.Parsec.Char (char, digit, space)

parseInteger :: Parsec String () Integer
parseInteger = read <$> many1 digit

parseSignedInteger :: Parsec String () Integer
parseSignedInteger = do
  neg <- option False (char '-' >> return True)
  int <- parseInteger
  return $ if neg then negate int else int

spaces :: Parsec String () String
spaces = many1 space

head' :: [a] -> Maybe a
head' [] = Nothing
head' (a : _) = Just a

decDigits :: [Char]
decDigits = "0123456789"

fromDecDigit :: Char -> Maybe Int
fromDecDigit c = elemIndex c decDigits

unsafeFromDecDigit :: Char -> Int
unsafeFromDecDigit = fromJust . fromDecDigit

hexDigits :: [Char]
hexDigits = "0123456789ABCDEF"

fromHexDigit :: Char -> Maybe Int
fromHexDigit c = elemIndex c hexDigits

unsafeFromHexDigit :: Char -> Int
unsafeFromHexDigit = fromJust . fromHexDigit

inBounds :: Ord a => a -> (a, a) -> Bool
inBounds x (xMin, xMax) = xMin <= x && x <= xMax

readBinary :: Num a => String -> a
readBinary s = go s 0
  where
    go [] n = n
    go ('0' : rest) n = go rest (2 * n)
    go ('1' : rest) n = go rest (2 * n + 1)
    go _ _ = undefined

showBinary :: Int -> [Char]
showBinary n = go n ""
  where
    go 0 "" = "0"
    go 0 s = s
    go n s = go (n `div` 2) (intToDigit (n `mod` 2) : s)

padLToLength :: Int -> Char -> [Char] -> [Char]
padLToLength n pad str
  | length str < n = padLToLength n pad (pad : str)
  | otherwise = str

countItems :: (Ord a) => [a] -> Map.Map a Integer
countItems = Map.fromListWith (+) . flip zip (repeat 1)

readGrid :: String -> Array.Array (Int, Int) Int
readGrid s =
  let ls = lines s
      height = length ls
      width = length $ head ls
   in Array.listArray ((0, 0), (height - 1, width - 1)) (map digitToInt $ concat ls)

adjacent :: (Ord a, Num a) => Array.Array (a, a) e -> (a, a) -> [(a, a)]
adjacent grid (x, y) =
  let ((xLow, yLow), (xHi, yHi)) = Array.bounds grid
   in filter
        (\(x', y') -> x' `inBounds` (xLow, xHi) && y' `inBounds` (yLow, yHi))
        [ (x - 1, y),
          (x + 1, y),
          (x, y - 1),
          (x, y + 1)
        ]
