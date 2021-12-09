module Main where

import Control.Monad (forM_)
import qualified Data.Map as M
import Days (runDay)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Supply a space-separated list of days to run"
    else forM_ args runDay
