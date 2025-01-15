module AOC202116 where

import Control.Monad.State(State,evalState,get,put)

import AOC

aoc = AOC {
    day="../../2021/input/16",
    aocTests=[
        AOCTest {
            testData="8A004A801A8002F478",
            testResult=Just "16",
            testResult2=Nothing
            },
        AOCTest {
            testData="620080001611562C8802118E34",
            testResult=Just "12",
            testResult2=Nothing
            },
        AOCTest {
            testData="C0015000016115A2E0802F182340",
            testResult=Just "23",
            testResult2=Nothing
            },
        AOCTest {
            testData="A0016C880162017C3686B18A3D4780",
            testResult=Just "31",
            testResult2=Nothing
            },
        AOCTest {
            testData="C200B40A82",
            testResult=Nothing,
            testResult2=Just "3"
            },
        AOCTest {
            testData="04005AC33890",
            testResult=Nothing,
            testResult2=Just "54"
            },
        AOCTest {
            testData="880086C3E88112",
            testResult=Nothing,
            testResult2=Just "7"
            },
        AOCTest {
            testData="CE00C43D881120",
            testResult=Nothing,
            testResult2=Just "9"
            },
        AOCTest {
            testData="D8005AC2A8F0",
            testResult=Nothing,
            testResult2=Just "1"
            },
        AOCTest {
            testData="F600BC2D8F",
            testResult=Nothing,
            testResult2=Just "0"
            },
        AOCTest {
            testData="9C005AC2F8F0",
            testResult=Nothing,
            testResult2=Just "0"
            },
        AOCTest {
            testData="9C0141080250320F1802104A08",
            testResult=Nothing,
            testResult2=Just "1"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

data Packet = Literal Int Int | Operator Int Int [Packet] deriving Show

parse = evalState parsePacket . parseHex

parseHex :: String -> [Int]
parseHex = concatMap p
  where
    p '0' = [0,0,0,0]
    p '1' = [0,0,0,1]
    p '2' = [0,0,1,0]
    p '3' = [0,0,1,1]
    p '4' = [0,1,0,0]
    p '5' = [0,1,0,1]
    p '6' = [0,1,1,0]
    p '7' = [0,1,1,1]
    p '8' = [1,0,0,0]
    p '9' = [1,0,0,1]
    p 'A' = [1,0,1,0]
    p 'B' = [1,0,1,1]
    p 'C' = [1,1,0,0]
    p 'D' = [1,1,0,1]
    p 'E' = [1,1,1,0]
    p 'F' = [1,1,1,1]

parsePacket :: State [Int] Packet
parsePacket = do
    version <- parseNumber 3
    typeID <- parseNumber 3
    if typeID == 4
      then do
        value <- parseLiteral 0
        return $ Literal version value
      else do
        subpackets <- parseSubpackets
        return $ Operator version typeID subpackets

parseNumber :: Int -> State [Int] Int
parseNumber nbits = do
    bits <- get
    let (numberBits,rest) = splitAt nbits bits
    put rest
    return $ evalBits 0 numberBits
  where
    evalBits accumulator [] = accumulator
    evalBits accumulator (bit:bits) = evalBits (accumulator*2+bit) bits

parseLiteral :: Int -> State [Int] Int
parseLiteral accumulator = do
    (flag:rest) <- get
    put rest
    n <- parseNumber 4
    if flag == 1
      then parseLiteral (16*accumulator+n)
      else return $ (16*accumulator+n)

parseSubpackets :: State [Int] [Packet]
parseSubpackets = do
    lengthTypeID <- parseNumber 1
    if lengthTypeID == 0
      then do
        nbits <- parseNumber 15
        bits <- get
        let (subpacketBits,rest) = splitAt nbits bits
        put subpacketBits
        subpackets <- p0 []
        put rest
        return subpackets
      else do
        npackets <- parseNumber 11
        p1 npackets []
  where
    p0 accumulator = do
        bits <- get
        if null bits
          then return $ reverse accumulator
          else do
            packet <- parsePacket
            p0 (packet:accumulator)

    p1 0 accumulator = return $ reverse accumulator
    p1 n accumulator = do
        packet <- parsePacket
        p1 (n-1) (packet:accumulator)

result (Literal version _) = version
result (Operator version _ subpackets) = version + sum (map result subpackets)

greaterthan :: [Int] -> Int
greaterthan [a,b] | a > b = 1 | otherwise = 0

lessthan :: [Int] -> Int
lessthan [a,b] | a < b = 1 | otherwise = 0

equalto :: [Int] -> Int
equalto [a,b] | a == b = 1 | otherwise = 0

result2 (Literal _ n) = n
result2 (Operator _ 0 subpackets) = sum $ map result2 subpackets
result2 (Operator _ 1 subpackets) = product $ map result2 subpackets
result2 (Operator _ 2 subpackets) = minimum $ map result2 subpackets
result2 (Operator _ 3 subpackets) = maximum $ map result2 subpackets
result2 (Operator _ 5 subpackets) = greaterthan $ map result2 subpackets
result2 (Operator _ 6 subpackets) = lessthan $ map result2 subpackets
result2 (Operator _ 7 subpackets) = equalto $ map result2 subpackets
