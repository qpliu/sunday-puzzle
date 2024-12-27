module AOC202423 where

import AOC

import Data.List(intercalate,sort)
import Data.Map(alter,(!))
import qualified Data.Map
import Data.Set(elems,empty,fromList,insert,isSubsetOf,member,singleton,size)

aoc = AOC {
    day="23",
    aocTests=[
        AOCTest {
            testData=unlines [
                "kh-tc",
                "qp-kh",
                "de-cg",
                "ka-co",
                "yn-aq",
                "qp-ub",
                "cg-tb",
                "vc-aq",
                "tb-ka",
                "wh-tc",
                "yn-cg",
                "kh-ub",
                "ta-co",
                "de-co",
                "tc-td",
                "tb-wq",
                "wh-td",
                "ta-ka",
                "td-qp",
                "aq-cg",
                "wq-ub",
                "ub-vc",
                "de-ta",
                "wq-aq",
                "wq-vc",
                "wh-yn",
                "ka-de",
                "kh-ta",
                "co-tc",
                "wh-qp",
                "tb-vc",
                "td-yn"
                ],
            testResult=Just "7",
            testResult2=Just "\"co,de,ka,ta\""
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

parse = foldr collect (empty,empty,Data.Map.empty) . lines
  where
    collect item (chief,nodes,graph) = (ch3,nodes2,graph3)
      where
        a = take 2 item
        b = drop 3 item
        nodes2 = insert a $ insert b nodes
        ch2 | take 1 a == "t" = insert a chief | otherwise = chief
        ch3 | take 1 b == "t" = insert b ch2 | otherwise = ch2
        graph2 = alter (add a) b graph
        graph3 = alter (add b) a graph2
    add node = Just . maybe (singleton node) (insert node)

result (chief,nodes,graph) =
    size $ fromList $ [sort [a,b,c] | a <- elems chief,
                                      b <- elems (graph!a),
                                      c <- elems (graph!b),
                                      c /= a, c < b,
                                      member c (graph!a)]

result2 (_,nodes,graph) =
    intercalate "," $ sort $ elems $ getBiggest (0,empty) $ elems nodes
  where
    getBiggest (_,biggestSet) [] = biggestSet
    getBiggest biggest@(biggestSize,biggestSet) (node:nodes)
      | neighborsSize <= biggestSize = getBiggest biggest nodes
      | fst maxSet <= biggestSize = getBiggest biggest nodes
      | otherwise = getBiggest maxSet nodes
      where
        neighbors = graph!node
        neighborsSize = size neighbors

        maxSet = maximum $ setsWith (singleton node) (elems neighbors)

        setsWith set [] = [(size set,set)]
        setsWith set (n:queue)
          | set `isSubsetOf` (graph!n) =
              setsWith (insert n set) queue ++ setsWith set queue
          | otherwise = setsWith set queue
