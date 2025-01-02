module AOC202316 where

import Data.Array(Array,bounds,inRange,(!))
import Data.Set(Set,empty,fromList,insert,member,size)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2023/input/16",
    aocTests=[
        AOCTest {
            testData=unlines [
                ".|...\\....",
                "|.-.\\.....",
                ".....|-...",
                "........|.",
                "..........",
                ".........\\",
                "..../.\\\\..",
                ".-.-/..|..",
                ".|....-|.\\",
                "..//.|...."
                ],
            testResult=Just "46",
            testResult2=Just "51"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse2da,
        pcodeParse2=const parse2da,
        pcodeTest=const result,
        pcodeTest2=result2,
        pcodeResult=const result,
        pcodeResult2=result2
        }
    }

activations :: ((Int,Int),(Int,Int)) -> Array (Int,Int) Char -> Int
activations entry grid = size $ Data.Set.map fst $ beam empty ([],entry)
  where
    inBounds = inRange (bounds grid)
    beam beams (queue,state@(xy@(x,y),dxy@(dx,dy)))
      | not (inBounds xy) = dequeue beams queue
      | member state beams = dequeue beams queue
      | otherwise = beam (insert state beams) (nextState (grid!xy))
      where
        dequeue beams [] = beams
        dequeue beams (state:queue) = beam beams (queue,state)
        nextState '.' = (queue,((x+dx,y+dy),dxy))
        nextState '|'
          | dx == 0 = (queue,((x+dx,y+dy),dxy))
          | otherwise = (((x,y-1),(0,-1)):queue,((x,y+1),(0,1)))
        nextState '-'
          | dy == 0 = (queue,((x+dx,y+dy),dxy))
          | otherwise = (((x-1,y),(-1,0)):queue,((x+1,y),(1,0)))
        nextState '/' = (queue,((x-dy,y-dx),(-dy,-dx)))
        nextState '\\' = (queue,((x+dy,y+dx),(dy,dx)))

result = activations ((0,0),(1,0))

result2 ncpu grid =
    maximum [
        parallelMapReduce ncpu (flip activations grid) maximum
            [((x,ymin),(0,1))  | x <- [xmin..xmax]],
        parallelMapReduce ncpu (flip activations grid) maximum
            [((x,ymax),(0,-1)) | x <- [xmin..xmax]],
        parallelMapReduce ncpu (flip activations grid) maximum
            [((xmin,y),(1,0))  | y <- [ymin..ymax]],
        parallelMapReduce ncpu (flip activations grid) maximum
            [((xmax,y),(-1,0)) | y <- [ymin..ymax]]
        ]
  where
    ((xmin,ymin),(xmax,ymax)) = bounds grid
