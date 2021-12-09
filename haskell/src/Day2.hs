{-# LANGUAGE NamedFieldPuns #-}

module Day2 where

import Text.Parsec
  ( ParseError,
    Parsec,
    many,
    newline,
    parse,
    string,
    (<|>),
  )
import Util (parseInteger, spaces)

data Command = Command CommandType Integer deriving (Show)

data CommandType = Forward | Down | Up deriving (Show)

parseCommandType :: Parsec String () CommandType
parseCommandType =
  (string "forward" >> return Forward)
    <|> (string "down" >> return Down)
    <|> (string "up" >> return Up)

parseCommand :: Parsec String () Command
parseCommand = do
  typ <- parseCommandType
  spaces
  Command typ <$> parseInteger

parseCommands :: Parsec String () [Command]
parseCommands = do
  many
    ( do
        cmd <- parseCommand
        newline
        return cmd
    )

parseInput :: String -> Either ParseError [Command]
parseInput = parse parseCommands ""

class Commandable t where
  empty :: t
  runCommand :: t -> Command -> t
  getDist :: t -> Integer
  getDepth :: t -> Integer

data Position = Position {pDist :: Integer, pDepth :: Integer}

instance Commandable Position where
  empty = Position {pDist = 0, pDepth = 0}
  getDist Position {pDist} = pDist
  getDepth Position {pDepth} = pDepth

  runCommand p@Position {pDist} (Command Forward n) = p {pDist = pDist + n}
  runCommand p@Position {pDepth} (Command Down n) = p {pDepth = pDepth + n}
  runCommand p@Position {pDepth} (Command Up n) = p {pDepth = pDepth - n}

data AimedPosition = AimedPosition {distance :: Integer, depth :: Integer, aim :: Integer}

instance Commandable AimedPosition where
  empty = AimedPosition {distance = 0, depth = 0, aim = 0}
  getDist AimedPosition {distance} = distance
  getDepth AimedPosition {depth} = depth

  runCommand a@AimedPosition {distance, depth, aim} (Command Forward n) =
    a
      { distance = distance + n,
        depth = depth + n * aim
      }
  runCommand a@AimedPosition {aim} (Command Down n) = a {aim = aim + n}
  runCommand a@AimedPosition {aim} (Command Up n) = a {aim = aim - n}

runCommands :: (Commandable a) => [Command] -> a
runCommands = foldl runCommand empty

part1 s =
  let p = runCommands s :: Position
   in getDepth p * getDist p

part2 s =
  let p = runCommands s :: AimedPosition
   in getDepth p * getDist p

runDay2 rawInput = do
  case parseInput rawInput of
    Left err -> print err
    Right input -> do
      putStrLn $ "Part 1: " ++ show (part1 input)
      putStrLn $ "Part 2: " ++ show (part2 input)
