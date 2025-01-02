module AOC202315 where

import Data.Char(ord)
import Data.Map(Map,adjust,fromList,toList)

import AOC

aoc = AOC {
    day="../../2023/input/15",
    aocTests=[
        AOCTest {
            testData=unlines [
                "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
                ],
            testResult=Just "1320",
            testResult2=Just "145"
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

parse :: String -> [String]
parse = filter (/= "") . lines . map uncomma
  where uncomma c | c == ',' = '\n' | otherwise = c

hash :: String -> Int
hash = foldl h 0 . map ord
  where h current c = (17*(current+c)) `mod` 256

result = sum . map hash

emptyBox :: Map Int [(String,Int)]
emptyBox = fromList $ zip [0..255] $ repeat []

process :: Map Int [(String,Int)] -> String -> Map Int [(String,Int)]
process boxes insn
  | op == '-' = adjust remove box boxes
  | op == '=' = adjust replace box boxes
  where
    (label,op:focalLen) = span (not . isOp) insn
    box = hash label
    isOp ch = ch == '-' || ch == '='
    remove [] = []
    remove (lens@(l,_):rest)
      | l == label = rest
      | otherwise = lens : remove rest
    replace [] = [(label,read focalLen)]
    replace (lens@(l,_):rest)
      | l == label = (label,read focalLen) : rest
      | otherwise = lens : replace rest

focusingPower :: (Int,[(String,Int)]) -> Int
focusingPower (box,lenses) = (box+1)*sum (zipWith (*) [1..] $ map snd $ lenses)

result2 = sum . map focusingPower . toList . foldl process emptyBox
