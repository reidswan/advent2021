module Days where

import Day1 (runDay1)
import Day10 (runDay10)
import Day2 (runDay2)
import System.TimeIt (timeIt)

runDay n = do
  rawInput <- readFile $ inputFile n
  timeIt $ runDay_ n rawInput

runDay_ "1" = runDay1
runDay_ "2" = runDay2
runDay_ "10" = runDay10
runDay_ n = \_ -> putStrLn $ "Day " ++ n ++ " not available"

inputFile s = "input/" ++ pad s ++ ".txt"
  where
    pad s
      | length s < 2 = pad ('0' : s)
      | otherwise = s