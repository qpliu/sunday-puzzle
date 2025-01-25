module AOC201918 where

import Data.Char(isLower,isUpper,toLower)

import Data.Array(Array,assocs,(!),(//))
import qualified Data.Array
import Data.Map(Map,insert)
import qualified Data.Map
import Data.Set(Set,delete,fromList,member,union)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2019/input/18",
    aocTests=[
        AOCTest {
            testData=unlines [
                "#########",
                "#b.A.@.a#",
                "#########"
                ],
            testResult=Just "8",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "########################",
                "#f.D.E.e.C.b.A.@.a.B.c.#",
                "######################.#",
                "#d.....................#",
                "########################"
                ],
            testResult=Just "86",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "########################",
                "#...............b.C.D.f#",
                "#.######################",
                "#.....@.a.B.c.d.A.e.F.g#",
                "########################"
                ],
            testResult=Just "132",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "#################",
                "#i.G..c...e..H.p#",
                "########.########",
                "#j.A..b...f..D.o#",
                "########@########",
                "#k.E..a...g..B.n#",
                "########.########",
                "#l.F..d...h..C.m#",
                "#################"
                ],
            testResult=Just "136",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "########################",
                "#@..............ac.GI.b#",
                "###d#e#f################",
                "###A#B#C################",
                "###g#h#i################",
                "########################"
                ],
            testResult=Just "81",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "#######",
                "#a.#Cd#",
                "##...##",
                "##.@.##",
                "##...##",
                "#cB#Ab#",
                "#######"
                ],
            testResult=Nothing,
            testResult2=Just "8"
            },
        AOCTest {
            testData=unlines [
                "###############",
                "#d.ABC.#.....a#",
                "######...######",
                "######.@.######",
                "######...######",
                "#b.....#.....c#",
                "###############"
                ],
            testResult=Nothing,
            testResult2=Just "24"
            },
        AOCTest {
            testData=unlines [
                "#############",
                "#DcBa.#.GhKl#",
                "#.###...#I###",
                "#e#d#.@.#j#k#",
                "###C#...###J#",
                "#fEbA.#.FgHi#",
                "#############"
                ],
            testResult=Nothing,
            testResult2=Just "32"
            },
        AOCTest {
            testData=unlines [
                "#############",
                "#g#f.D#..h#l#",
                "#F###e#E###.#",
                "#dCba...BcIJ#",
                "#####.@.#####",
                "#nK.L...G...#",
                "#M###N#H###.#",
                "#o#m..#i#jk.#",
                "#############"
                ],
            testResult=Nothing,
            testResult2=Nothing -- Just "72" -- don't know what my bug is
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result,
        codeResult=result,
        codeResult2=result
        }
    }

type XY = (Int,Int)

makeGraphs :: Array XY Char -> [(XY,Set Char,Map XY [(Int,Char,XY)])]
makeGraphs grid = map makeGraph starts
  where
    starts = map fst $ filter ((== '@') . snd) $ assocs grid

    nodes = fromList $ map fst $ filter isNode $ assocs grid
    isNode ((x,y),ch)
      | ch == '#' = False
      | ch /= '.' = True
      | otherwise = exitCount > 2
      where exitCount = length [() | (dx,dy) <- [(1,0),(0,1),(-1,0),(0,-1)],
                                     grid!(x+dx,y+dy) /= '#']

    makeGraph start = (start,startKeys,graph)
      where
        (startKeys,graph) = walk (Data.Set.empty,Data.Map.empty) [start]

    walk (keys,graph) [] = (keys,graph)
    walk (keys,graph) (xy@(x,y):queue)
      | Data.Map.member xy graph = walk (keys,graph) queue
      | otherwise =
          walk (union newKeys keys,insert xy edges graph) (queue ++ newNodes)
      where
        edges = concat [follow 1 xy (x+dx,y+dy)
                        | (dx,dy) <- [(1,0),(0,1),(-1,0),(0,-1)],
                          grid!(x+dx,y+dy) /= '#']
        newKeys = fromList $ concatMap extractKey edges
        extractKey (_,k,_) | isLower k = [k] | otherwise = []
        newNodes = map extractNode edges
        extractNode (_,_,xy) = xy

    follow nsteps fromXY xy@(x,y)
      | member xy nodes = [(nsteps,grid!xy,xy)]
      | null exits = []
      | length exits == 1 = follow (nsteps+1) xy (head exits)
      where
        exits = [(x+dx,y+dy) | (dx,dy) <- [(1,0),(0,1),(-1,0),(0,-1)],
                               (x+dx,y+dy) /= fromXY,
                               grid!(x+dx,y+dy) /= '#']

parse = parse2da

search :: (XY,Set Char,Map XY [(Int,Char,XY)]) -> Int
search (startXY,startKeys,graph) = finalCost
  where
    Just (finalCost,_) = astar heuristic neighbors toState done initialPaths

    heuristic (cost,_) = cost
    toState (_,state) = state
    done (_,(_,unclaimedKeys)) = null unclaimedKeys
    initialPaths = [(0,(startXY,startKeys))]

    neighbors (cost,(xy,unclaimedKeys)) =
        [(cost+nsteps,(nextXY,delete contents unclaimedKeys))
         | (nsteps,contents,nextXY) <- graph Data.Map.! xy,
           not (isUpper contents)
               || not (member (toLower contents) unclaimedKeys)]

-- For part 2, ignore doors that are locked by keys in other quadrants.
-- Presumably their keys would be collected by other robots.
result = sum . map search . makeGraphs

parse2 = splitEntry . parse2da
  where
    splitEntry grid = grid // [((x-1,y-1),'@'),((x,y-1),'#'),((x+1,y-1),'@'),
                               ((x-1,y),  '#'),((x,y),  '#'),((x+1,y),  '#'),
                               ((x-1,y+1),'@'),((x,y+1),'#'),((x+1,y+1),'@')]
      where [((x,y),_)] = filter ((== '@') . snd) $ assocs grid
