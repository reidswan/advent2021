module Day3 where

import Data.Bits (clearBit, complement, setBit, shiftL, shiftR, (.&.))
import Data.List (partition)
import Data.Word
import Util (readBinary)

bitMask :: Word
bitMask = complement $ shiftL (complement 0) 12

bitHigh :: Int -> Word -> Bool
bitHigh n w = shiftR w n .&. 1 == 1

bitLow :: Int -> Word -> Bool
bitLow n = not . bitHigh n

getBit = bitHigh

setBitTo :: Int -> Bool -> Word -> Word
setBitTo n True w = setBit w n
setBitTo n False w = clearBit w n

mostCommonBit :: Int -> [Word] -> Bool
mostCommonBit n ws =
  let (his, los) = partition (bitHigh n) ws
   in length his >= length los

leastCommonBit :: Int -> [Word] -> Bool
leastCommonBit n = not . mostCommonBit n

computeGamma :: [Word] -> Word
computeGamma ws = go ws [0 .. 11] 0
  where
    go _ [] n = n
    go ws (head : rest) n =
      let mcb = mostCommonBit head ws
          n' = setBitTo head mcb n
       in go ws rest n'

epsilonFromGamma :: Word -> Word
epsilonFromGamma g = bitMask .&. complement g

gasRating :: (Int -> [Word] -> Bool) -> [Word] -> Word
gasRating selector ws = go ws [11, 10 .. 0]
  where
    go [] _ = undefined
    go [w] _ = w
    go ws [] = undefined
    go ws (n : rest) =
      let desiredBit = selector n ws
          survivors = filter (\w -> getBit n w == desiredBit) ws
       in go survivors rest

oxygenRating :: [Word] -> Word
oxygenRating = gasRating mostCommonBit

co2Rating :: [Word] -> Word
co2Rating = gasRating leastCommonBit

part1 :: [Word] -> Word
part1 input =
  let gamma = computeGamma input
      epsilon = epsilonFromGamma gamma
   in gamma * epsilon

part2 :: [Word] -> Word
part2 input =
  let o2 = oxygenRating input
      co2 = co2Rating input
   in o2 * co2

parseInput :: String -> [Word]
parseInput = map readBinary . lines

runDay3 :: String -> IO ()
runDay3 rawInput = do
  let input = parseInput rawInput
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)