data Packet = Literal Int Int | Packet Int Int [Packet] deriving (Eq,Show)

toBits :: String -> [Bool]
toBits = concatMap xToBits
  where
    xToBits '0' = [False,False,False,False]
    xToBits '1' = [False,False,False,True]
    xToBits '2' = [False,False,True, False]
    xToBits '3' = [False,False,True, True]
    xToBits '4' = [False,True, False,False]
    xToBits '5' = [False,True, False,True]
    xToBits '6' = [False,True, True, False]
    xToBits '7' = [False,True, True, True]
    xToBits '8' = [True, False,False,False]
    xToBits '9' = [True, False,False,True]
    xToBits 'A' = [True, False,True, False]
    xToBits 'B' = [True, False,True, True]
    xToBits 'C' = [True, True, False,False]
    xToBits 'D' = [True, True, False,True]
    xToBits 'E' = [True, True, True, False]
    xToBits 'F' = [True, True, True, True]
    xToBits _ = []

toNum :: Integral a => [Bool] -> a
toNum bits = sum $ map snd $ filter fst $ zip bits [2^(length bits - i) | i <- [1..]]

packet :: String -> Packet
packet = fst . parsePacket . toBits

parsePacket :: [Bool] -> (Packet,[Bool])
parsePacket (v1:v2:v3:t1:t2:t3:bits)
  | packetType == 4 = parseLiteralPacket version 0 bits
  | take 1 bits == [False] = parseLengthTypeID0 version packetType (drop 1 bits)
  | take 1 bits == [True] = parseLengthTypeID1 version packetType (drop 1 bits)
  | otherwise = error (show bits)
  where
    version = toNum [v1,v2,v3]
    packetType = toNum [t1,t2,t3]
parsePacket bits = error (show bits)

parseLiteralPacket :: Int -> Int -> [Bool] -> (Packet,[Bool])
parseLiteralPacket version n (False:b1:b2:b3:b4:rest) = (Literal version (16*n+toNum [b1,b2,b3,b4]),rest)
parseLiteralPacket version n (True:b1:b2:b3:b4:rest) = parseLiteralPacket version (16*n+toNum [b1,b2,b3,b4]) rest
parseLiteralPacket version n bits = error $ show (version,n,bits)

parseLengthTypeID0 :: Int -> Int -> [Bool] -> (Packet,[Bool])
parseLengthTypeID0 version packetType bits = (Packet version packetType subpackets,remainingBits)
  where
    (lengthBits,afterLengthBits) = splitAt 15 bits
    (packetBits,remainingBits) = splitAt (toNum lengthBits) afterLengthBits
    subpackets = parseSubpackets packetBits
    parseSubpackets bits
      | null bits = []
      | otherwise = let (p,b) = parsePacket bits in p : parseSubpackets b

parseLengthTypeID1 :: Int -> Int -> [Bool] -> (Packet,[Bool])
parseLengthTypeID1 version packetType bits = (Packet version packetType (subpackets),remainingBits)
  where
    (countBits,packetBits) = splitAt 11 bits
    (subpackets,remainingBits) = parseSubpackets (toNum countBits) [] packetBits
    parseSubpackets n revSubpackets bits
      | n == 0 = (reverse revSubpackets,bits)
      | otherwise = let (p,b) = parsePacket bits in parseSubpackets (n-1) (p:revSubpackets) b

versionSum :: Packet -> Int
versionSum (Literal version _) = version
versionSum (Packet version _ subpackets) = version + sum (map versionSum subpackets)

test :: ()
test
  | packet "D2FE28" /= Literal 6 2021 = error "a"
  | packet "38006F45291200" /= Packet 1 6 [Literal 6 10,Literal 2 20] = error "b"
  | packet "EE00D40C823060" /= Packet 7 3 [Literal 2 1,Literal 4 2,Literal 1 3] = error "c"
  | versionSum (packet "8A004A801A8002F478") /= 16 = error "d"
  | versionSum (packet "620080001611562C8802118E34") /= 12 = error "e"
  | versionSum (packet "C0015000016115A2E0802F182340") /= 23 = error "f"
  | versionSum (packet "A0016C880162017C3686B18A3D4780") /= 31 = error "g"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map (versionSum . packet) . words) $ readFile "input/16.txt"

value :: Packet -> Int
value (Literal _ n) = n
value (Packet _ 0 subpackets) = sum $ map value subpackets
value (Packet _ 1 subpackets) = product $ map value subpackets
value (Packet _ 2 subpackets) = minimum $ map value subpackets
value (Packet _ 3 subpackets) = maximum $ map value subpackets
value (Packet _ 5 [s1,s2]) = if value s1 > value s2 then 1 else 0
value (Packet _ 6 [s1,s2]) = if value s1 < value s2 then 1 else 0
value (Packet _ 7 [s1,s2]) = if value s1 == value s2 then 1 else 0

test2 :: ()
test2
  | (value . packet) "C200B40A82" /= 3 = error "a"
  | (value . packet) "04005AC33890" /= 54 = error "b"
  | (value . packet) "880086C3E88112" /= 7 = error "c"
  | (value . packet) "CE00C43D881120" /= 9 = error "d"
  | (value . packet) "D8005AC2A8F0" /= 1 = error "e"
  | (value . packet) "F600BC2D8F" /= 0 = error "f"
  | (value . packet) "9C005AC2F8F0" /= 0 = error "g"
  | (value . packet) "9C0141080250320F1802104A08" /= 1 = error "h"
  | otherwise = ()

part2 :: IO [Int]
part2 = fmap (map (value . packet) . words) $ readFile "input/16.txt"
