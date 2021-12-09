module Util where

import Text.Parsec hiding (spaces)
import Text.Parsec.Char hiding (spaces)

parseInteger :: Parsec String () Integer
parseInteger = read <$> many1 digit

spaces :: Parsec String () String
spaces = many1 space