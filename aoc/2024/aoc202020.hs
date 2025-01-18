module AOC202020 where

import Control.Monad(msum)
import Data.Array(assocs,bounds,(!))
import Data.Set(Set,difference,elems,empty,fromList,intersection,member,size,union,unions)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2020/input/20",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Tile 2311:",
                "..##.#..#.",
                "##..#.....",
                "#...##..#.",
                "####.#...#",
                "##.##.###.",
                "##...#.###",
                ".#.#.#..##",
                "..#....#..",
                "###...#.#.",
                "..###..###",
                "",
                "Tile 1951:",
                "#.##...##.",
                "#.####...#",
                ".....#..##",
                "#...######",
                ".##.#....#",
                ".###.#####",
                "###.##.##.",
                ".###....#.",
                "..#.#..#.#",
                "#...##.#..",
                "",
                "Tile 1171:",
                "####...##.",
                "#..##.#..#",
                "##.#..#.#.",
                ".###.####.",
                "..###.####",
                ".##....##.",
                ".#...####.",
                "#.##.####.",
                "####..#...",
                ".....##...",
                "",
                "Tile 1427:",
                "###.##.#..",
                ".#..#.##..",
                ".#.##.#..#",
                "#.#.#.##.#",
                "....#...##",
                "...##..##.",
                "...#.#####",
                ".#.####.#.",
                "..#..###.#",
                "..##.#..#.",
                "",
                "Tile 1489:",
                "##.#.#....",
                "..##...#..",
                ".##..##...",
                "..#...#...",
                "#####...#.",
                "#..#.#.#.#",
                "...#.#.#..",
                "##.#...##.",
                "..##.##.##",
                "###.##.#..",
                "",
                "Tile 2473:",
                "#....####.",
                "#..#.##...",
                "#.##..#...",
                "######.#.#",
                ".#...#.#.#",
                ".#########",
                ".###.#..#.",
                "########.#",
                "##...##.#.",
                "..###.#.#.",
                "",
                "Tile 2971:",
                "..#.#....#",
                "#...###...",
                "#.#.###...",
                "##.##..#..",
                ".#####..##",
                ".#..####.#",
                "#..#.#..#.",
                "..####.###",
                "..#.#.###.",
                "...#.#.#.#",
                "",
                "Tile 2729:",
                "...#.#.#.#",
                "####.#....",
                "..#.#.....",
                "....#..#.#",
                ".##..##.#.",
                ".#.####...",
                "####.#.#..",
                "##.####...",
                "##..#.##..",
                "#.##...##.",
                "",
                "Tile 3079:",
                "#.#.#####.",
                ".#..######",
                "..#.......",
                "######....",
                "####.#..#.",
                ".#...#.##.",
                "#.#####.##",
                "..#.###...",
                "..#.......",
                "..#.###..."
                ],
            testResult=Just "20899048083289",
            testResult2=Just "273"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

type Grid = Set (Int,Int)
type Tile = (Int,Int,[Set Int],Grid)

parse :: String -> [Tile]
parse = map parseTile . parseBlankLineSeparated
  where
    parseTile (name:tileLines) = (head $ parseInts name,xmax,edges,tile)
      where
        grid = parse2da $ unlines tileLines
        (_,(xmax,_)) = bounds grid
        tile = fromList $ map fst $ filter ((== '#') . snd) $ assocs grid
        edges = [
            fromList [t | t <- [0..xmax], grid!(t,0) == '#'],
            fromList [t | t <- [0..xmax], grid!(xmax-t,0) == '#'],
            fromList [t | t <- [0..xmax], grid!(t,xmax) == '#'],
            fromList [t | t <- [0..xmax], grid!(xmax-t,xmax) == '#'],
            fromList [t | t <- [0..xmax], grid!(0,t) == '#'],
            fromList [t | t <- [0..xmax], grid!(0,xmax-t) == '#'],
            fromList [t | t <- [0..xmax], grid!(xmax,t) == '#'],
            fromList [t | t <- [0..xmax], grid!(xmax,xmax-t) == '#']
            ]

borders :: [Tile] -> Tile -> [Tile]
borders tiles (tileID,_,edges,_) =
    [tile | tile@(otherTileID,_,otherEdges,_) <- tiles,
            otherTileID /= tileID,
            size (intersection (fromList edges) (fromList otherEdges)) > 0]

cornerIDs :: Int -> [Tile] -> [Int]
cornerIDs ncpu tiles = parallelMapReduce ncpu toCornerIDs concat tiles
  where
    toCornerIDs tile@(tileID,_,_,_)
      | length (borders tiles tile) == 2 = [tileID]
      | otherwise = []

result ncpu = product . cornerIDs ncpu

orientations :: (Int,Grid) -> [Grid]
orientations (tileSize,points) = [
    points,
    Data.Set.map (\ (x,y) -> (tileSize-x,y)) points,
    Data.Set.map (\ (x,y) -> (x,tileSize-y)) points,
    Data.Set.map (\ (x,y) -> (tileSize-x,tileSize-y)) points,
    Data.Set.map (\ (x,y) -> (y,x)) points,
    Data.Set.map (\ (x,y) -> (tileSize-y,x)) points,
    Data.Set.map (\ (x,y) -> (y,tileSize-x)) points,
    Data.Set.map (\ (x,y) -> (tileSize-y,tileSize-x)) points
    ]

translate :: Grid -> (Int,Int) -> Grid
translate points (dx,dy) = Data.Set.map translatePoint points
  where translatePoint (x,y) = (x+dx,y+dy)

removeEdges :: Int -> Grid -> Grid
removeEdges xmax = Data.Set.map closeGaps . Data.Set.filter notEdge
  where
    notEdge (x,y) = x `mod` xmax /= 0 && y `mod` xmax /= 0
    closeGaps (x,y) = (x-(x`div`xmax),y-(y`div`xmax))

reassemble :: [Tile] -> (Int,Grid)
reassemble tiles@((_,xmax,_,_):_) =
    (nside*(xmax-1),translate (removeEdges xmax reassembled) (-1,-1))
  where
    nside = round $ sqrt $ fromIntegral $ length tiles
    tileNeighbors = zip tiles $ map (borders tiles) tiles
    Just reassembled = msum (map topLeft tileNeighbors)

    topLeft :: (Tile,[Tile]) -> Maybe Grid
    topLeft ((_,_,_,tgrid),nbors)
      | length nbors == 2 =
          msum [topRow 1 otgrid ongrid nnbors
                | otgrid <- orientations (xmax,tgrid),
                  nbor@(_,_,_,ngrid) <- nbors,
                  Just nnbors <- [lookup nbor tileNeighbors],
                  ongrid <- orientations (xmax,ngrid)]
      | otherwise = Nothing

    topRow :: Int -> Grid -> Grid -> [Tile] -> Maybe Grid
    topRow ix grid tgrid nbors
      | ix >= nside = rowRightEdge 1 grid tgrid nbors
      | and [member (ix*xmax,y) grid == member (0,y) tgrid | y <- [0..xmax]] =
          msum [topRow (ix+1) nextgrid ongrid nnbors
                | nbor@(_,_,_,ngrid) <- nbors,
                  Just nnbors <- [lookup nbor tileNeighbors],
                  ongrid <- orientations (xmax,ngrid)]
      | otherwise = Nothing
      where nextgrid = union grid (translate tgrid (ix*xmax,0))

    rowRightEdge :: Int -> Grid -> Grid -> [Tile] -> Maybe Grid
    rowRightEdge iy grid tgrid nbors
      | iy >= nside = Just grid
      | and [member ((nside-1)*xmax+x,iy*xmax) grid == member (x,0) tgrid
             | x <- [0..xmax]] =
          msum [rowRight (nside-2) iy nextgrid ongrid nnbors
                | nbor@(_,_,_,ngrid) <- nbors,
                  Just nnbors <- [lookup nbor tileNeighbors],
                  ongrid <- orientations (xmax,ngrid)]
      | otherwise = Nothing
      where nextgrid = union grid (translate tgrid ((nside-1)*xmax,iy*xmax))

    rowRight :: Int -> Int -> Grid -> Grid -> [Tile] -> Maybe Grid
    rowRight ix iy grid tgrid nbors
      | ix < 0 = rowLeftEdge (iy+1) grid tgrid nbors
      | and [member (ix*xmax+x,iy*xmax) grid == member (x,0) tgrid
             | x <- [0..xmax]] &&
        and [member ((ix+1)*xmax,iy*xmax+y) grid == member (xmax,y) tgrid
             | y <- [0..xmax]] =
          msum [rowRight (ix-1) iy nextgrid ongrid nnbors
                | nbor@(_,_,_,ngrid) <- nbors,
                  Just nnbors <- [lookup nbor tileNeighbors],
                  ongrid <- orientations (xmax,ngrid)]
      | otherwise = Nothing
      where nextgrid = union grid (translate tgrid (ix*xmax,iy*xmax))

    rowLeftEdge :: Int -> Grid -> Grid -> [Tile] -> Maybe Grid
    rowLeftEdge iy grid tgrid nbors
      | iy >= nside = Just grid
      | and [member (x,iy*xmax) grid == member (x,0) tgrid | x <- [0..xmax]] =
          msum [rowLeft 1 iy nextgrid ongrid nnbors
                | nbor@(_,_,_,ngrid) <- nbors,
                  Just nnbors <- [lookup nbor tileNeighbors],
                  ongrid <- orientations (xmax,ngrid)]
      | otherwise = Nothing
      where nextgrid = union grid (translate tgrid (0,iy*xmax))

    rowLeft :: Int -> Int -> Grid -> Grid -> [Tile] -> Maybe Grid
    rowLeft ix iy grid tgrid nbors
      | ix >= nside = rowRightEdge (iy+1) grid tgrid nbors
      | and [member (ix*xmax+x,iy*xmax) grid == member (x,0) tgrid
             | x <- [0..xmax]] &&
        and [member (ix*xmax,iy*xmax+y) grid == member (0,y) tgrid
             | y <- [0..xmax]] =
          msum [rowLeft (ix+1) iy nextgrid ongrid nnbors
                | nbor@(_,_,_,ngrid) <- nbors,
                  Just nnbors <- [lookup nbor tileNeighbors],
                  ongrid <- orientations (xmax,ngrid)]
      | otherwise = Nothing
      where nextgrid = union grid (translate tgrid (ix*xmax,iy*xmax))

monster :: Set (Int,Int)
monster = fromList $ map toXY $ filter ((== '#') . snd) $ assocs grid
  where
    grid = parse2da $ unlines [
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
        ]
    toXY ((x,y),_) = (x,y-1)

search :: Int -> Set (Int,Int) -> Int
search ncpu grid = size (difference grid monsters)
  where
    monsters = parallelMapReduce ncpu checkMonster unions $ elems grid
    checkMonster xy
      | translatedMonster == intersection translatedMonster grid =
          translatedMonster
      | otherwise = empty
      where translatedMonster = translate monster xy

result2 ncpu = minimum . map (search ncpu) . orientations . reassemble
