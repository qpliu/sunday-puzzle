module AOC202508 where

import Data.List(sort)
import Data.Map(delete,empty,member,insert,toList,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="08",
    aocTests=[
        AOCTest {
            testData=unlines [
                "162,817,812",
                "57,618,57",
                "906,360,560",
                "592,479,940",
                "352,342,300",
                "466,668,158",
                "542,29,236",
                "431,825,988",
                "739,650,466",
                "52,470,668",
                "216,146,977",
                "819,987,18",
                "117,168,530",
                "805,96,715",
                "346,949,466",
                "970,615,88",
                "941,993,340",
                "862,61,35",
                "984,92,344",
                "425,690,689"
            ],
            testResult=Just "40",
            testResult2=Just "25272"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 10,
        codeTest2=result2,
        codeResult=result 1000,
        codeResult2=result2
        }
    }

parse = toCoords . parseInts
  where
    toCoords (x:y:z:rest) = (x,y,z) : toCoords rest
    toCoords _ = []

dist2 (x1,y1,z1) (x2,y2,z2) = (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2

result npairs input =
    product $ take 3 $ reverse $ sort $ map snd $ toList sizes
  where
    (sizes,circuits) =
        foldr collect (empty,empty)
            $ take npairs
            $ sort [(dist2 a b,a,b) | a <- input, b <- input, a < b]
    collect (_,a,b) (sizes,circuits)
      | havea && haveb && ak == bk = (sizes,circuits)
      | havea && haveb =
          (insert ak (sizes!ak + sizes!bk) (delete bk sizes),
           Data.Map.map (\ k -> if k == bk then ak else k) circuits)
      | havea = (insert ak (sizes!ak + 1) sizes,insert b ak circuits)
      | haveb = (insert bk (sizes!bk + 1) sizes,insert a bk circuits)
      | otherwise = (insert a 2 sizes,insert b a (insert a a circuits))
      where
        havea = member a circuits
        haveb = member b circuits
        ak = circuits!a
        bk = circuits!b

result2 input = 
    collect 0 empty empty $ sort [(dist2 a b,a,b) | a <- input, b <- input,
                                                    a < b]
  where
    nboxes = length input
    collect last2 sizes circuits ((_,a@(ax,_,_),b@(bx,_,_)):rest)
      | not (null sizes) && maximum sizes == nboxes = last2
      | havea && haveb && ak == bk = collect (ax*bx) sizes circuits rest
      | havea && haveb =
            collect (ax*bx)
                    (insert ak (sizes!ak + sizes!bk) (delete bk sizes))
                    (Data.Map.map (\ k -> if k == bk then ak else k) circuits)
                    rest
      | havea = collect (ax*bx)
                        (insert ak (sizes!ak + 1) sizes)
                        (insert b ak circuits)
                        rest
      | haveb = collect (ax*bx)
                        (insert bk (sizes!bk + 1) sizes)
                        (insert a bk circuits)
                        rest
      | otherwise = collect (ax*bx)
                            (insert a 2 sizes)
                            (insert b a (insert a a circuits))
                            rest
      where
        havea = member a circuits
        haveb = member b circuits
        ak = circuits!a
        bk = circuits!b
