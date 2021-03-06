module Days where

import Day1 (runDay1)
import Day10 (runDay10)
import Day11 (runDay11)
import Day14 (runDay14)
import Day15 (runDay15)
import Day16 (runDay16)
import Day17 (runDay17)
import Day18 (runDay18)
import Day19 (runDay19)
import Day2 (runDay2)
import Day20 (runDay20)
import Day21 (runDay21)
import Day22 (runDay22)
import Day3 (runDay3)
import System.TimeIt (timeIt)

runDay n = do
  rawInput <- readFile $ inputFile n
  timeIt $ runDay_ n rawInput

runDay_ "1" = runDay1
runDay_ "2" = runDay2
runDay_ "3" = runDay3
runDay_ "10" = runDay10
runDay_ "11" = runDay11
runDay_ "14" = runDay14
runDay_ "15" = runDay15
runDay_ "16" = runDay16
runDay_ "17" = runDay17
runDay_ "18" = runDay18
runDay_ "19" = runDay19
runDay_ "20" = runDay20
runDay_ "21" = runDay21
runDay_ "22" = runDay22
runDay_ n = \_ -> putStrLn $ "Day " ++ n ++ " not available"

inputFile s = "input/" ++ pad s ++ ".txt"
  where
    pad s
      | length s < 2 = pad ('0' : s)
      | otherwise = s