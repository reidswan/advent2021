{-# LANGUAGE NamedFieldPuns #-}

module Day16 where

import Control.Monad (forM)
import Debug.Trace (traceId)
import Text.Parsec (ParseError, Parsec, char, count, eof, many, many1, oneOf, parse, unexpected)
import Util (padLToLength, readBinary, showBinary, unsafeFromHexDigit)

type Parser a = Parsec String () a

data OpType = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo deriving (Show)

data PacketType = TLiteral | TOperator OpType deriving (Show)

data Header = Header {packetVersion :: Int, packetType :: PacketType} deriving (Show)

data LengthType = BitCount | SubpacketCount deriving (Show)

data Length = Length {lengthType :: LengthType, amount :: Int} deriving (Show)

data Contents = Literal Int | Operator OpType [Packet] deriving (Show)

data Packet = Packet {header :: Header, contents :: Contents} deriving (Show)

runOp :: OpType -> [Int] -> Int
runOp Sum = sum
runOp Product = product
runOp Minimum = minimum
runOp Maximum = maximum
runOp GreaterThan = \(a : b : _) -> if a > b then 1 else 0
runOp LessThan = \(a : b : _) -> if a < b then 1 else 0
runOp EqualTo = \(a : b : _) -> if a == b then 1 else 0

combineHexList :: [Int] -> Int
combineHexList = foldl (\acc v -> 16 * acc + v) 0

parseNBits :: Int -> Parser Int
parseNBits n = do
  bitsS <- count n $ oneOf "01"
  return $ readBinary bitsS

parsePacketType :: Parser PacketType
parsePacketType = do
  bits <- parseNBits 3
  case bits of
    0 -> return $ TOperator Sum
    1 -> return $ TOperator Product
    2 -> return $ TOperator Minimum
    3 -> return $ TOperator Maximum
    4 -> return TLiteral
    5 -> return $ TOperator GreaterThan
    6 -> return $ TOperator LessThan
    7 -> return $ TOperator EqualTo
    s -> unexpected $ "unknown operator type " ++ show s

parseHeader :: Parser Header
parseHeader = do
  packetVersion <- parseNBits 3
  packetType <- parsePacketType
  return $ Header {packetVersion, packetType}

parseLiteral :: Parser Contents
parseLiteral = parseChunk []
  where
    parseChunk vals = do
      continue <- flag
      val <- parseNBits 4
      if continue
        then parseChunk (val : vals)
        else return $ Literal $ combineHexList $ reverse (val : vals)

parseOperator :: OpType -> Parser Contents
parseOperator op = do
  len <- parseLength
  let amt = amount len
  contents <- case lengthType len of
    SubpacketCount -> forM [1 .. amt] (const parsePacket)
    BitCount -> do
      bits <- count amt $ oneOf "01"
      case parse parsePackets "" bits of
        Left err -> unexpected $ show err
        Right contents -> return contents
  return $ Operator op contents

parsePackets :: Parser [Packet]
parsePackets = do
  pkts <- many1 parsePacket
  eof
  return pkts

flag :: Parser Bool
flag = do
  f <- oneOf "01"
  return $ f == '1'

parseLengthType :: Parser LengthType
parseLengthType = do
  isSubCount <- flag
  return $ if isSubCount then SubpacketCount else BitCount

parseLength :: Parser Length
parseLength = do
  lengthType <- parseLengthType
  let bitN = case lengthType of
        BitCount -> 15
        SubpacketCount -> 11
  amount <- parseNBits bitN
  return $ Length {lengthType, amount}

parsePacket :: Parser Packet
parsePacket = do
  header <- parseHeader
  contents <- case packetType header of
    TLiteral -> parseLiteral
    TOperator op -> parseOperator op
  return $ Packet {header, contents}

parseEnvelopePacket :: Parser Packet
parseEnvelopePacket = do
  packet <- parsePacket
  many $ char '0'
  return packet

parseInput :: String -> Either ParseError Packet
parseInput hexInput =
  let nums = map unsafeFromHexDigit hexInput
      binaryInput = concatMap (padLToLength 4 '0' . showBinary) nums
   in parse parseEnvelopePacket "" binaryInput

versionSum :: Packet -> Int
versionSum Packet {header = Header {packetVersion}, contents} = case contents of
  Literal _ -> packetVersion
  Operator _ pkts -> foldl (\acc pkt -> acc + versionSum pkt) packetVersion pkts

evaluate :: Packet -> Int
evaluate Packet {contents = Literal i} = i
evaluate Packet {contents = Operator op pkts} = runOp op $ map evaluate pkts

runDay16 :: String -> IO ()
runDay16 rawInput = do
  case parseInput rawInput of
    Left err -> print err
    Right input -> do
      putStrLn $ "Part 1: " ++ show (versionSum input)
      putStrLn $ "Part 2: " ++ show (evaluate input)
