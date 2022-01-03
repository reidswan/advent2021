module Day21 where

import Data.Array ((!))
import qualified Data.Array as Array
import Debug.Trace (traceShowId)
import Text.Parsec (ParseError, Parsec, digit, newline, oneOf, optional, parse, string)
import Util (parseInteger, spaces)

type Parser a = Parsec String () a

newtype DeterministicDie = DeterministicDie {next :: Integer} deriving (Show)

data Player = Player {position :: Integer, score :: Integer, turn :: Turn} deriving (Show)

data GameState = GameState
  { player1 :: Player,
    player2 :: Player,
    die :: DeterministicDie,
    currTurn :: Turn,
    turnCount :: Integer
  }
  deriving (Show)

roll :: DeterministicDie -> (Integer, DeterministicDie)
roll DeterministicDie {next} =
  let next' = (next `mod` 100) + 1
   in (next, DeterministicDie {next = next'})

rollN :: Integer -> DeterministicDie -> (Integer, DeterministicDie)
rollN n DeterministicDie {next} =
  let vals = [next .. next + n - 1]
      next' = next + n
   in (sum vals, DeterministicDie {next = next'})

movePlayerIfOnTurn :: Turn -> Player -> Integer -> Player
movePlayerIfOnTurn t p@Player {position, score, turn} steps
  | turn == t =
    let position' = moveForward position steps
        score' = score + position'
     in Player {position = position', score = score', turn}
  | otherwise = p

stepGame :: GameState -> GameState
stepGame GameState {player1, player2, die, currTurn, turnCount} =
  let (steps, die') = rollN 3 die
      turn' = nextTurn currTurn
      p1' = movePlayerIfOnTurn currTurn player1 steps
      p2' = movePlayerIfOnTurn currTurn player2 steps
   in GameState {player1 = p1', player2 = p2', die = die', currTurn = turn', turnCount = turnCount + 1}

moveForward :: Integer -> Integer -> Integer
moveForward curr amt = ((curr + amt - 1) `mod` 10) + 1

data Turn = Player1 | Player2 deriving (Eq, Show)

nextTurn :: Turn -> Turn
nextTurn Player1 = Player2
nextTurn Player2 = Player1

player_ :: Parser Player
player_ = do
  string "Player "
  player <- oneOf "12"
  let turn = if player == '1' then Player1 else Player2
  string " starting position: "
  position <- parseInteger
  optional newline
  return $ Player {score = 0, turn, position}

gameState_ :: Parser GameState
gameState_ = do
  player1 <- player_
  player2 <- player_
  let die = DeterministicDie {next = 1}
      currTurn = Player1
      turnCount = 0
  return $ GameState {player1, player2, currTurn, die, turnCount}

parseInput :: String -> Either ParseError GameState
parseInput = parse gameState_ ""

runGameTo :: Integer -> GameState -> GameState
runGameTo maxScore g@GameState {player1, player2}
  | score player1 >= maxScore || score player2 >= maxScore = g
  | otherwise = runGameTo maxScore (stepGame g)

dieRollCount :: GameState -> Integer
dieRollCount GameState {turnCount} = 3 * turnCount

part1 :: GameState -> Integer
part1 g =
  let g'@GameState {player1, player2} = runGameTo 1000 g
      losingScore = min (score player1) (score player2)
   in losingScore * dieRollCount g'

-- how many distinct rolls of 3 dice can result in the given sum
universeCount :: Integer -> Integer
universeCount 3 = 1
universeCount 4 = 3
universeCount 5 = 6
universeCount 6 = 7
universeCount 7 = 6
universeCount 8 = 3
universeCount 9 = 1
universeCount _ = undefined

cumulativeRolls :: [Integer]
cumulativeRolls = [3 .. 9]

-- lazy dynamic programming a la https://jelv.is/blog/Lazy-Dynamic-Programming/
winCounts :: Player -> Player -> (Integer, Integer)
winCounts player1 player2 = go (score player1) (position player1) (score player2) (position player2)
  where
    go score1 pos1 score2 pos2
      | score1 >= 21 = (1, 0)
      | score2 >= 21 = (0, 1)
      | otherwise =
        let nextPos1s = map (moveForward pos1) cumulativeRolls
            nextScore1s = map (+ score1) nextPos1s
            rollPosScore1 = zip cumulativeRolls $ zip nextPos1s nextScore1s
            nextStates = [(roll, (score2, pos2, score1', pos1')) | (roll, (pos1', score1')) <- rollPosScore1]
            wins = [(universeCount roll, dynamicStore ! nextState) | (roll, nextState) <- nextStates]
         in foldl (\(c1, c2) (count, (w2, w1)) -> (c1 + count * w1, c2 + count * w2)) (0, 0) wins
    dynamicStore = Array.listArray ((0, 1, 0, 1), (30, 10, 30, 10)) [go s1 p1 s2 p2 | s1 <- [0 .. 30], p1 <- [1 .. 10], s2 <- [0 .. 30], p2 <- [1 .. 10]]

part2 :: GameState -> Integer
part2 GameState {player1, player2} = max w1 w2
  where
    (w1, w2) = winCounts player1 player2

runDay21 :: String -> IO ()
runDay21 rawInput =
  case parseInput rawInput of
    Left err -> print err
    Right g -> do
      putStrLn $ "Part 1: " ++ show (part1 g)
      putStrLn $ "Part 2: " ++ show (part2 g)