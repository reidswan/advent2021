module Util where

import Text.Parsec hiding (spaces)
import Text.Parsec.Char hiding (spaces)
import Data.Foldable (find)
import Data.List (findIndex, elemIndex)
import Data.Maybe (fromJust)

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
