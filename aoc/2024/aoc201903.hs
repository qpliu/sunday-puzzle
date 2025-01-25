module AOC201903 where

import Data.Map(Map,alter,empty,member,(!))

import AOC

aoc = AOC {
    day="../../2019/input/03",
    aocTests=[
        AOCTest {
            testData=unlines [
                "R8,U5,L5,D3",
                "U7,R6,D4,L4"
                ],
            testResult=Just "6",
            testResult2=Just "30"
            },
        AOCTest {
            testData=unlines [
                "R75,D30,R83,U83,L12,D49,R71,U7,L72",
                "U62,R66,U55,R34,D71,R55,D58,R83"
                ],
            testResult=Just "159",
            testResult2=Just "610"
            },
        AOCTest {
            testData=unlines [
                "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
                "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                ],
            testResult=Just "135",
            testResult2=Just "410"
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

parse :: String -> [[(Char,Int)]]
parse = map (map p . words) . lines . map uncomma
  where
    uncomma ',' = ' '
    uncomma ch = ch
    p (d:count) = (d,read count)

segment :: (Char,Int) -> ((Int,Int),Int) -> ([((Int,Int),Int)],((Int,Int),Int))
segment ('R',n) ((x,y),i) = ([((x+dx,y),i+dx) | dx <- [1..n]],((x+n,y),i+n))
segment ('D',n) ((x,y),i) = ([((x,y+dy),i+dy) | dy <- [1..n]],((x,y+n),i+n))
segment ('L',n) ((x,y),i) = ([((x-dx,y),i+dx) | dx <- [1..n]],((x-n,y),i+n))
segment ('U',n) ((x,y),i) = ([((x,y-dy),i+dy) | dy <- [1..n]],((x,y-n),i+n))

makeWire :: [(Char,Int)] -> Map (Int,Int) Int
makeWire = fst . foldl addSegment (empty,((0,0),0))
  where
    addSegment (wire,start) seg = (nextWire,nextStart)
      where
        (steps,nextStart) = segment seg start
        nextWire = foldr addStep wire steps
        addStep (xy,i) = alter (Just . maybe i (min i)) xy

crossWires :: ((Int,Int) -> Int -> Int -> Int)
           -> Map (Int,Int) Int -> [(Char,Int)] -> [Int]
crossWires metric wire = followSegments ((0,0),0)
  where
    followSegments _ [] = []
    followSegments start (seg:segs) = followSteps steps
      where
        (steps,nextStart) = segment seg start
        followSteps [] = followSegments nextStart segs
        followSteps ((xy,i):steps)
          | member xy wire = metric xy i (wire!xy) : followSteps steps
          | otherwise = followSteps steps

dist :: (Int,Int) -> Int -> Int -> Int
dist (x,y) _ _ = abs x + abs y

result [wire1,wire2] = minimum $ crossWires dist (makeWire wire1) wire2

steps :: (Int,Int) -> Int -> Int -> Int
steps _ s1 s2 = s1 + s2

result2 [wire1,wire2] = minimum $ crossWires steps (makeWire wire1) wire2
