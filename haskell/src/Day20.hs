module Day20 where

import Data.Array
import qualified Data.Array as Arr ((!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId)
import Text.Parsec (ParseError, Parsec, endBy, eof, many1, newline, oneOf, parse)
import Util (enumerate, readBinary', valsSortedByKey)

type Coord = (Integer, Integer)

type Parser a = Parsec String () a

data Image = Image
  { alg :: Array Int Bool,
    img :: Map.Map Coord Bool,
    iter :: Integer
  }

showBit True = '#'
showBit False = '.'

showAssocs :: [(Coord, Bool)] -> String
showAssocs cs = go cs Map.empty
  where
    go [] m = display m
    go (((x, _), v) : rest) m = go rest $ Map.alter (\s -> Just $ showBit v : fromMaybe [] s) x m

    display m = foldl (\a b -> a ++ '\n' : reverse b) "" (valsSortedByKey m) ++ "\n"

instance Show Image where
  show i =
    let area@((xMin, _), (xMax, _)) = relevantArea i
        rowLen = xMax - xMin
        bitVals = mapArea area (bitAt i)
        imgStr = showAssocs bitVals
     in "iter = " ++ show (iter i) ++ imgStr

emptyBit :: Image -> Bool
emptyBit = odd . iter

bitLine :: Parser [Bool]
bitLine = do
  pixelArr <- many1 $ oneOf "#."
  newline
  return $ map (== '#') pixelArr

imageSample :: Parser (Map.Map Coord Bool)
imageSample = do
  lines <- many1 bitLine
  let lines' = [((i, j), b) | (i, line) <- enumerate lines, (j, b) <- enumerate line]
  return $ Map.fromList lines'

image :: Parser Image
image = do
  algoList <- bitLine
  let alg = listArray (0, 511) algoList
  newline
  img <- imageSample
  eof
  return $ Image {alg, img, iter = 0}

parseInput :: String -> Either ParseError Image
parseInput = parse image ""

bitAt :: Image -> Coord -> Bool
bitAt i c = fromMaybe (emptyBit i) $ Map.lookup c $ img i

relevantArea :: Image -> (Coord, Coord)
relevantArea i@Image {img} =
  let empty = emptyBit i
      keys = Map.keys $ Map.filter (/= empty) img
      xs = map fst keys
      ys = map snd keys
   in ((minimum xs - 2, minimum ys - 2), (maximum xs + 2, maximum ys + 2))

adjacent :: Coord -> [Coord]
adjacent (x, y) = [(x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1]]

enhancementIndex :: Image -> Coord -> Int
enhancementIndex i c =
  let adj = map (bitAt i) $ adjacent c
   in readBinary' adj

enhancedValue :: Image -> Coord -> Bool
enhancedValue i c =
  let index = enhancementIndex i c
   in alg i ! index

mapArea :: (Coord, Coord) -> (Coord -> a) -> [(Coord, a)]
mapArea ((xMin, yMin), (xMax, yMax)) f = [((x, y), f (x, y)) | x <- [xMin .. xMax], y <- [yMin .. yMax]]

enhanceImage :: Image -> Image
enhanceImage i =
  let area = relevantArea i
      assocs = mapArea area (enhancedValue i)
      img' = Map.fromList assocs
      iter' = iter i + 1
   in i {img = img', iter = iter'}

enhanceImageN :: Int -> Image -> Image
enhanceImageN n i
  | n > 0 = enhanceImageN (n - 1) (enhanceImage i)
  | otherwise = i

litPixels :: Image -> Int
litPixels = length . Map.filter id . img

part1 :: Image -> Int
part1 i =
  let i' = enhanceImageN 2 i
   in litPixels i'

part2 :: Image -> Int
part2 i =
  let i' = enhanceImageN 50 i
   in litPixels i'

runDay20 :: String -> IO ()
runDay20 rawInput = do
  let input = parseInput rawInput
  case input of
    Left err -> print err
    Right i -> do
      putStrLn $ "Part 1: " ++ show (part1 i)
      putStrLn $ "Part 2: " ++ show (part2 i)