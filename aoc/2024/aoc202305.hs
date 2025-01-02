module AOC202305 where

import AOC

aoc = AOC {
    day="../../2023/input/05",
    aocTests=[
        AOCTest {
            testData=unlines [
                "seeds: 79 14 55 13",
                "",
                "seed-to-soil map:",
                "50 98 2",
                "52 50 48",
                "",
                "soil-to-fertilizer map:",
                "0 15 37",
                "37 52 2",
                "39 0 15",
                "",
                "fertilizer-to-water map:",
                "49 53 8",
                "0 11 42",
                "42 0 7",
                "57 7 4",
                "",
                "water-to-light map:",
                "88 18 7",
                "18 25 70",
                "",
                "light-to-temperature map:",
                "45 77 23",
                "81 45 19",
                "68 64 13",
                "",
                "temperature-to-humidity map:",
                "0 69 1",
                "1 0 69",
                "",
                "humidity-to-location map:",
                "60 56 37",
                "56 93 4"
                ],
            testResult=Just "35",
            testResult2=Just "46"
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

parse :: String -> ([Int],[[(Int,Int,Int)]])
parse = p . map words . lines
  where
    p (("seeds:":seeds):[]:mappings) = (map read seeds,pMappings [] mappings)
    pMappings mapping [] = [mapping]
    pMappings mapping ([]:mappings) = mapping : pMappings [] mappings
    pMappings mapping ([_,"map:"]:mappings) = pMappings [] mappings
    pMappings mapping ([dest,src,len]:mappings) =
        pMappings ((read dest,read src,read len):mapping) mappings

applyMap :: Int -> [(Int,Int,Int)] -> Int
applyMap n [] = n
applyMap n ((dest,src,len):mapping)
  | n >= src && n < src+len = dest+n-src
  | otherwise = applyMap n mapping

applyMaps :: [[(Int,Int,Int)]] -> Int -> Int
applyMaps maps n = foldl applyMap n maps

result (seeds,maps) = minimum $ map (applyMaps maps) seeds

toRanges :: [Int] -> [(Int,Int)]
toRanges [] = []
toRanges (start:len:rest) = (start,len):toRanges rest

applyMap2 :: [(Int,Int,Int)] -> (Int,Int) -> [(Int,Int)]
applyMap2 [] (start,len1)
  | len1 <= 0 = []
  | otherwise = [(start,len1)]
applyMap2 ((dest,src,len):mapping) (start,len1)
  | start >= src+len || start+len1 <= src = applyMap2 mapping (start,len1)
  | start >= src && start+len1 <= src+len = [(dest+start-src,len1)]
  | start >= src = (dest+start-src,len-(start-src)) : applyMap2 mapping (src+len,start+len1-src-len)
  | start+len1 <= src+len = (dest,start+len1-src) : applyMap2 mapping (start,src-start)
  | otherwise = (dest,len) : applyMap2 mapping (start,src-start) ++ applyMap2 mapping (src+len,start+len1-src-len)

applyMaps2 :: [[(Int,Int,Int)]] -> [(Int,Int)] -> [(Int,Int)]
applyMaps2 [] ranges = ranges
applyMaps2 (mapping:mappings) ranges = applyMaps2 mappings $ concatMap (applyMap2 mapping) ranges

result2 (seeds,maps) = fst $ minimum $ applyMaps2 maps (toRanges seeds)
