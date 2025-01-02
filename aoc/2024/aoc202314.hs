module AOC202314 where

import Data.Map(Map,empty,findWithDefault,insert,keys,toList,(!))
import qualified Data.Map
import Data.Set(Set,elems)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2023/input/14",
    aocTests=[
        AOCTest {
            testData=unlines [
                "O....#....",
                "O.OO#....#",
                ".....##...",
                "OO.#O....O",
                ".O.....O#.",
                "O.#..O.#.#",
                "..O..#O..O",
                ".......O..",
                "#....###..",
                "#OO..#...."
                ],
            testResult=Just "136",
            testResult2=Just "64"
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

parse :: String -> ((Int,Int,[((Int,Int),(Int,Int,Int,Int))]),Set (Int,Int))
parse input = ((xmax,ymax,spaces),rocks)
  where
    grid = parse2d input
    (xmax,ymax) = maximum $ keys grid
    rocks = Data.Set.fromList $ map fst $ filter ((== 'O') . snd) $ toList grid
    spaces = [((x,y),countSpaces (x,y)) | x <- [-1,xmax+1], y <- [0..ymax]]
          ++ [((x,y),countSpaces (x,y)) | x <- [0..xmax], y <- [-1,ymax+1]]
          ++ [((x,y),countSpaces (x,y)) | ((x,y),c) <- toList grid, c == '#']
    countSpaces (x,y) =
        (count (0,-1) 1,count (1,0) 1,count (0,1) 1,count (-1,0) 1)
      where
        count xy@(dx,dy) n
          | findWithDefault '#' (x+n*dx,y+n*dy) grid == '#' = n-1
          | otherwise = count xy (n+1)

totalLoad :: Int -> Set (Int,Int) -> Int
totalLoad ymax rocks = sum $ map load $ elems rocks
  where load (_,y) = ymax+1-y

tiltN :: (Int,Int,[((Int,Int),(Int,Int,Int,Int))]) -> Set (Int,Int)
      -> Set (Int,Int)
tiltN (xmax,ymax,spaces) rocks = Data.Set.fromList $ concatMap roll spaces
  where
    roll ((x,y),(_,_,nspaces,_)) = [(x,y+dy) | dy <- [1..nrocks]]
      where
        nrocks = length [() | dy <- [1..nspaces],
                              Data.Set.member (x,y+dy) rocks]

tiltE :: (Int,Int,[((Int,Int),(Int,Int,Int,Int))]) -> Set (Int,Int)
      -> Set (Int,Int)
tiltE (xmax,ymax,spaces) rocks = Data.Set.fromList $ concatMap roll spaces
  where
    roll ((x,y),(_,_,_,nspaces)) = [(x-dx,y) | dx <- [1..nrocks]]
      where
        nrocks = length [() | dx <- [1..nspaces],
                              Data.Set.member (x-dx,y) rocks]

tiltS :: (Int,Int,[((Int,Int),(Int,Int,Int,Int))]) -> Set (Int,Int)
      -> Set (Int,Int)
tiltS (xmax,ymax,spaces) rocks = Data.Set.fromList $ concatMap roll spaces
  where
    roll ((x,y),(nspaces,_,_,_)) = [(x,y-dy) | dy <- [1..nrocks]]
      where
        nrocks = length [() | dy <- [1..nspaces],
                              Data.Set.member (x,y-dy) rocks]

tiltW :: (Int,Int,[((Int,Int),(Int,Int,Int,Int))]) -> Set (Int,Int)
      -> Set (Int,Int)
tiltW (xmax,ymax,spaces) rocks = Data.Set.fromList $ concatMap roll spaces
  where
    roll ((x,y),(_,nspaces,_,_)) = [(x+dx,y) | dx <- [1..nrocks]]
      where
        nrocks = length [() | dx <- [1..nspaces],
                              Data.Set.member (x+dx,y) rocks]

result (plat@(_,ymax,_),rocks) = totalLoad ymax $ tiltN plat rocks

findCycle :: Ord a => (a -> a) -> a -> (Int,Int,Map Int a)
findCycle f a = search a 0 empty empty
  where
    search a i table history =
        maybe searchNext found $ Data.Map.lookup a history
      where
        searchNext = search (f a) (i+1) (insert i a table) (insert a i history)
        found n = (n,i-n,table)

result2 (plat@(_,ymax,_),rocks) = totalLoad ymax (history!i)
  where
    (first,cycleLen,history) =
        findCycle (tiltE plat . tiltS plat . tiltW plat . tiltN plat) rocks
    i = (1000000000 - first) `mod` cycleLen + first
