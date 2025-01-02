module AOC202308 where

import Data.Map(Map,empty,fromList,insert,keys,member,(!))

import AOC

aoc = AOC {
    day="../../2023/input/08",
    aocTests=[
        AOCTest {
            testData=unlines [
                "RL",
                "",
                "AAA = (BBB, CCC)",
                "BBB = (DDD, EEE)",
                "CCC = (ZZZ, GGG)",
                "DDD = (DDD, DDD)",
                "EEE = (EEE, EEE)",
                "GGG = (GGG, GGG)",
                "ZZZ = (ZZZ, ZZZ)"
                ],
            testResult=Just "2",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "LLR",
                "",
                "AAA = (BBB, BBB)",
                "BBB = (AAA, ZZZ)",
                "ZZZ = (ZZZ, ZZZ)"
                ],
            testResult=Just "6",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "LR",
                "",
                "11A = (11B, XXX)",
                "11B = (XXX, 11Z)",
                "11Z = (11B, XXX)",
                "22A = (22B, XXX)",
                "22B = (22C, 22C)",
                "22C = (22Z, 22Z)",
                "22Z = (22B, 22B)",
                "XXX = (XXX, XXX)"
                ],
            testResult=Nothing,
            testResult2=Just "6"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=const result,
        pcodeTest2=result2,
        pcodeResult=const result,
        pcodeResult2=result2
        }
    }

parse = p . lines
  where
    p (insns:"":nodes) = (insns,fromList $ map parseNode nodes)
    parseNode (n1:n2:n3:' ':'=':' ':'(':l1:l2:l3:',':' ':r1:r2:r3:')':_) =
        ([n1,n2,n3],([l1,l2,l3],[r1,r2,r3]))

follow (insns,graph) (node,[]) = follow (insns,graph) (node,insns)
follow (insns,graph) (node,('L':rest)) = (fst (graph!node),rest)
follow (insns,graph) (node,('R':rest)) = (snd (graph!node),rest)

result input = length $ takeWhile (/= "ZZZ") $ fmap fst $ iterate (follow input) ("AAA",[])

starts = filter ((== 'A') . last) . keys . snd

-- There is only one Z node in each loop in my input.
findLoop :: (String,Map String (String,String)) -> String -> (Int,Int)
findLoop network node = find 0 empty (node,[])
  where
    find n visited item@(node,insns)
      | last node == 'Z' && member item visited = (visited!item,n-visited!item)
      | otherwise = find (n+1) (insert item n visited) (follow network item)

result2 ncpu network =
    fst $ parallelMapReduce ncpu (findLoop network) (foldr convergences (1,1))
        $ starts network
