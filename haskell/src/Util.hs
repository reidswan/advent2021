module Util where

import Data.Char (intToDigit)
import Data.Foldable (find)
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromJust)
import Text.Parsec hiding (spaces)
import Text.Parsec.Char hiding (spaces)

parseInteger :: Parsec String () Integer
parseInteger = read <$> many1 digit

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
