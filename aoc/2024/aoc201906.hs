module AOC201906 where

import Data.Map(Map,alter,empty,fromList,(!))

import AOC

aoc = AOC {
    day="../../2019/input/06",
    aocTests=[
        AOCTest {
            testData=unlines [
                "COM)B",
                "B)C",
                "C)D",
                "D)E",
                "E)F",
                "B)G",
                "G)H",
                "D)I",
                "E)J",
                "J)K",
                "K)L"
                ],
            testResult=Just "42",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "COM)B",
                "B)C",
                "C)D",
                "D)E",
                "E)F",
                "B)G",
                "G)H",
                "D)I",
                "E)J",
                "J)K",
                "K)L",
                "K)YOU",
                "I)SAN"
                ],
            testResult=Nothing,
            testResult2=Just "4"
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

parse = map (p . span (/= ')')) . lines
  where p (a,')':b) = (a,b)

result orbits = sum table
  where
    table = fromList $ ("COM",0):map makeTable orbits
    makeTable (a,b) = (b,1+table!a)

makeGraph :: [(String,String)] -> Map String [String]
makeGraph = foldr addEdge empty
  where
    addEdge (a,b) =
        alter (Just . maybe [a] (a:)) b . alter (Just . maybe [b] (b:)) a

search :: Map String [String] -> Int
search graph = finalSteps
  where
    Just (finalSteps,_) =
        astar fst neighbors snd ((== "SAN") . snd) [(-2,"YOU")]
    neighbors (steps,loc) = map ((,) (steps+1)) (graph!loc)

result2 = search . makeGraph
