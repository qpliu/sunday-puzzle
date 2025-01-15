module AOC202112 where

import Data.Char(isUpper)
import Data.Map(Map,alter,(!))
import qualified Data.Map
import Data.Set(Set,elems,insert,member,singleton)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2021/input/12",
    aocTests=[
        AOCTest {
            testData=unlines [
                "start-A",
                "start-b",
                "A-c",
                "A-b",
                "b-d",
                "A-end",
                "b-end"
                ],
            testResult=Just "10",
            testResult2=Just "36"
            },
        AOCTest {
            testData=unlines [
                "dc-end",
                "HN-start",
                "start-kj",
                "dc-start",
                "dc-HN",
                "LN-dc",
                "HN-end",
                "kj-sa",
                "kj-HN",
                "kj-dc"
                ],
            testResult=Just "19",
            testResult2=Just "103"
            },
        AOCTest {
            testData=unlines [
                "fs-end",
                "he-DX",
                "fs-he",
                "start-DX",
                "pj-DX",
                "end-zg",
                "zg-sl",
                "zg-pj",
                "pj-he",
                "RW-he",
                "fs-DX",
                "pj-RW",
                "zg-RW",
                "start-pj",
                "he-WI",
                "zg-he",
                "pj-fs",
                "start-RW"
                ],
            testResult=Just "226",
            testResult2=Just "3509"
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

parse = Data.Map.map elems . foldr connect Data.Map.empty
                           . map (fmap tail . span (/= '-')) . lines
  where
    connect ("start",a) graph = add "start" a graph
    connect (a,"start") graph = add "start" a graph
    connect ("end",a) graph = add a "end" graph
    connect (a,"end") graph = add a "end" graph
    connect (a,b) graph = add a b $ add b a graph
    add a b graph = alter (Just . maybe (singleton b) (insert b)) a graph

big :: String -> Bool
big (c:_) = isUpper c

countPaths :: Int -> [(Set String,Bool,String)] -> Map String [String] -> Int
countPaths n [] graph = n
countPaths n ((_,_,"end"):queue) graph = countPaths (n+1) queue graph
countPaths n ((visited,canRevisit,cave):queue) graph =
    countPaths n (nexts ++ queue) graph
  where
    nextVisited = insert cave visited
    nexts = [(nextVisited,canRevisit && nextCanRevisit next,next)
             | next <- graph!cave,
               big next || not (member next visited) || canRevisit]
    nextCanRevisit next = big next || not (member next visited)

result = countPaths 0 [(Data.Set.empty,False,"start")]

result2 = countPaths 0 [(Data.Set.empty,True,"start")]
