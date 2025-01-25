module AOC201920 where

import Data.Array(Array,assocs,bounds,(!))
import Data.Char(isAlpha)
import Data.Map(Map,adjust,alter,empty,fromList)
import qualified Data.Map
import Data.Set(member)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2019/input/20",
    aocTests=[
        AOCTest {
            testData=unlines [
                "         A           ",
                "         A           ",
                "  #######.#########  ",
                "  #######.........#  ",
                "  #######.#######.#  ",
                "  #######.#######.#  ",
                "  #######.#######.#  ",
                "  #####  B    ###.#  ",
                "BC...##  C    ###.#  ",
                "  ##.##       ###.#  ",
                "  ##...DE  F  ###.#  ",
                "  #####    G  ###.#  ",
                "  #########.#####.#  ",
                "DE..#######...###.#  ",
                "  #.#########.###.#  ",
                "FG..#########.....#  ",
                "  ###########.#####  ",
                "             Z       ",
                "             Z       "
                ],
            testResult=Just "23",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "                   A               ",
                "                   A               ",
                "  #################.#############  ",
                "  #.#...#...................#.#.#  ",
                "  #.#.#.###.###.###.#########.#.#  ",
                "  #.#.#.......#...#.....#.#.#...#  ",
                "  #.#########.###.#####.#.#.###.#  ",
                "  #.............#.#.....#.......#  ",
                "  ###.###########.###.#####.#.#.#  ",
                "  #.....#        A   C    #.#.#.#  ",
                "  #######        S   P    #####.#  ",
                "  #.#...#                 #......VT",
                "  #.#.#.#                 #.#####  ",
                "  #...#.#               YN....#.#  ",
                "  #.###.#                 #####.#  ",
                "DI....#.#                 #.....#  ",
                "  #####.#                 #.###.#  ",
                "ZZ......#               QG....#..AS",
                "  ###.###                 #######  ",
                "JO..#.#.#                 #.....#  ",
                "  #.#.#.#                 ###.#.#  ",
                "  #...#..DI             BU....#..LF",
                "  #####.#                 #.#####  ",
                "YN......#               VT..#....QG",
                "  #.###.#                 #.###.#  ",
                "  #.#...#                 #.....#  ",
                "  ###.###    J L     J    #.#.###  ",
                "  #.....#    O F     P    #.#...#  ",
                "  #.###.#####.#.#####.#####.###.#  ",
                "  #...#.#.#...#.....#.....#.#...#  ",
                "  #.#####.###.###.#.#.#########.#  ",
                "  #...#.#.....#...#.#.#.#.....#.#  ",
                "  #.###.#####.###.###.#.#.#######  ",
                "  #.#.........#...#.............#  ",
                "  #########.###.###.#############  ",
                "           B   J   C               ",
                "           U   P   P               "
                ],
            testResult=Just "58",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "             Z L X W       C                 ",
                "             Z P Q B       K                 ",
                "  ###########.#.#.#.#######.###############  ",
                "  #...#.......#.#.......#.#.......#.#.#...#  ",
                "  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  ",
                "  #.#...#.#.#...#.#.#...#...#...#.#.......#  ",
                "  #.###.#######.###.###.#.###.###.#.#######  ",
                "  #...#.......#.#...#...#.............#...#  ",
                "  #.#########.#######.#.#######.#######.###  ",
                "  #...#.#    F       R I       Z    #.#.#.#  ",
                "  #.###.#    D       E C       H    #.#.#.#  ",
                "  #.#...#                           #...#.#  ",
                "  #.###.#                           #.###.#  ",
                "  #.#....OA                       WB..#.#..ZH",
                "  #.###.#                           #.#.#.#  ",
                "CJ......#                           #.....#  ",
                "  #######                           #######  ",
                "  #.#....CK                         #......IC",
                "  #.###.#                           #.###.#  ",
                "  #.....#                           #...#.#  ",
                "  ###.###                           #.#.#.#  ",
                "XF....#.#                         RF..#.#.#  ",
                "  #####.#                           #######  ",
                "  #......CJ                       NM..#...#  ",
                "  ###.#.#                           #.###.#  ",
                "RE....#.#                           #......RF",
                "  ###.###        X   X       L      #.#.#.#  ",
                "  #.....#        F   Q       P      #.#.#.#  ",
                "  ###.###########.###.#######.#########.###  ",
                "  #.....#...#.....#.......#...#.....#.#...#  ",
                "  #####.#.###.#######.#######.###.###.#.#.#  ",
                "  #.......#.......#.#.#.#.#...#...#...#.#.#  ",
                "  #####.###.#####.#.#.#.#.###.###.#.###.###  ",
                "  #.......#.....#.#...#...............#...#  ",
                "  #############.#.#.###.###################  ",
                "               A O F   N                     ",
                "               A A D   M                     "
                ],
            testResult=Nothing,
            testResult2=Just "396"
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

type XY = (Int,Int)

makePortals :: Array XY Char -> Map String [XY]
makePortals grid = foldr collect empty $ assocs grid
  where
    (_,(xmax,ymax)) = bounds grid
    collect ((x,y),ch) table
      | x == 1 && isAlpha ch =
          alter (Just . maybe [(x+1,y)] ((x+1,y):)) [grid!(x-1,y),ch] table
      | x == xmax-1 && isAlpha ch =
          alter (Just . maybe [(x-1,y)] ((x-1,y):)) [ch,grid!(x+1,y)] table
      | y == 1 && isAlpha ch =
          alter (Just . maybe [(x,y+1)] ((x,y+1):)) [grid!(x,y-1),ch] table
      | y == ymax-1 && isAlpha ch =
          alter (Just . maybe [(x,y-1)] ((x,y-1):)) [ch,grid!(x,y+1)] table
      | x > 0 && x < xmax && isAlpha ch && grid!(x-1,y) == '.' =
          alter (Just . maybe [(x-1,y)] (++[(x-1,y)])) [ch,grid!(x+1,y)] table
      | x > 0 && x < xmax && isAlpha ch && grid!(x+1,y) == '.' =
          alter (Just . maybe [(x+1,y)] (++[(x+1,y)])) [grid!(x-1,y),ch] table
      | y > 0 && y < ymax && isAlpha ch && grid!(x,y-1) == '.' =
          alter (Just . maybe [(x,y-1)] (++[(x,y-1)])) [ch,grid!(x,y+1)] table
      | y > 0 && y < ymax && isAlpha ch && grid!(x,y+1) == '.' =
          alter (Just . maybe [(x,y+1)] (++[(x,y+1)])) [grid!(x,y-1),ch] table
      | otherwise = table

makeNodes :: Array XY Char -> [XY]
makeNodes grid = map fst $ filter isNode $ assocs grid
  where
    isNode ((x,y),ch)
      | ch /= '.' = False
      | otherwise =
          not (null [() | (dx,dy) <- [(1,0),(0,1),(-1,0),(0,-1)],
                          isAlpha (grid!(x+dx,y+dy))])
              || 2 < length [() | (dx,dy) <- [(1,0),(0,1),(-1,0),(0,-1)],
                                  grid!(x+dx,y+dy) == '.']

makeGraph :: Array XY Char -> (XY,XY,Map XY [(Int,Int,XY)])
makeGraph grid = (aa,zz,graph)
  where
    [aa] = portals Data.Map.!"AA"
    [zz] = portals Data.Map.!"ZZ"
    graph = addPortals $ fromList $ map makeEdges $ Data.Set.elems nodes

    nodes = Data.Set.fromList $ makeNodes grid
    portals = makePortals grid

    makeEdges xy@(x,y) =
        (xy,concat [follow xy 1 xy xy1
                    | xy1 <- [(x+1,y),(x,y+1),(x-1,y),(x,y-1)],
                      grid!xy1 == '.'])

    follow :: XY -> Int -> XY -> XY -> [(Int,Int,XY)]
    follow startXY nsteps lastXY xy@(x,y)
      | member xy nodes = [(nsteps,0,xy)]
      | length nextXYs == 1 = follow startXY (nsteps+1) xy (head nextXYs)
      | otherwise = []
      where
        nextXYs = [nextXY | nextXY <- [(x+1,y),(x,y+1),(x-1,y),(x,y-1)],
                            nextXY /= startXY, nextXY /= lastXY,
                            grid!nextXY == '.']

    addPortals g = foldr addPortal g $ Data.Map.elems portals

    addPortal [_] g = g
    addPortal [xy1,xy2] g =
        adjust ((1,-1,xy2):) xy1 $ adjust ((1,1,xy1):) xy2 g

parse :: String -> (XY,XY,Map XY [(Int,Int,XY)])
parse = makeGraph . parse2da

search :: Bool -> XY -> XY -> Map XY [(Int,Int,XY)] -> Int
search hasLevels start end graph = finalCost
  where
    Just finalCost
      | hasLevels = fmap fst $ astar fst neighbors2 snd done2 [(0,(start,0))]
      | otherwise = fmap fst $ astar fst neighbors snd done [(0,start)]
    neighbors (cost,xy) =
        [(cost+steps,nextXY) | (steps,dlevel,nextXY) <- graph Data.Map.!xy]
    done (_,loc) = loc == end

    neighbors2 (cost,(xy,level)) =
        [(cost+steps,(nextXY,level+dlevel))
         | (steps,dlevel,nextXY) <- graph Data.Map.!xy,
           level+dlevel >= 0]
    done2 (_,loc) = loc == (end,0)

result :: (XY,XY,Map XY [(Int,Int,XY)]) -> Int
result (start,end,graph) = search False start end graph

result2 :: (XY,XY,Map XY [(Int,Int,XY)]) -> Int
result2 (start,end,graph) = search True start end graph
