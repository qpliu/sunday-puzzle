module AOC201917 where

import Data.Array(Array,bounds,indices,(!))
import Data.Char(chr,ord)
import Data.List(intercalate)
import Data.Map(Map,fromList,member,toList)
import qualified Data.Map
import Data.Set(Set,elems,empty,insert)
import qualified Data.Set
import Data.Vector.Unboxed(Vector,(//))

import AOC
import AOC2019

aoc = AOC {
    day="../../2019/input/17",
    aocTests=[],
    aocCode=Code {
        codeParse=parseIntCode,
        codeParse2=parseIntCode,
        codeTest=undefined,
        codeTest2=undefined,
        codeResult=result,
        codeResult2=result2
        }
    }

type XY = (Int,Int)

makeGrid :: Vector Int -> Array XY Char
makeGrid = parse2da . map chr . intCode []

xings :: Array XY Char -> [XY]
xings grid = filter isXing $ indices grid
  where
    (_,(xmax,ymax)) = bounds grid
    isXing (x,y) = 0 < x && x < xmax && 0 < y && y < ymax
                && and [grid!(x+dx,y+dy) == '#' | dx <- [-1,0,1],
                                                  dy <- [-1,0,1],
                                                  dx == 0 || dy == 0]

result = sum . map (uncurry (*)) . xings . makeGrid

-- Assume no U-turns are needed.  Assume that there are two dead-ends,
-- and the robot is at one of them.  That constrains up to two of the
-- movement functions.
--
-- Assume that the movement function out of the start cannot be the
-- same as the movement function into the other dead end.
--
-- Assume each movement function begins with a turn, alternates between
-- turns and forward motions, and ends with a forward motion, since
-- being able to break up a straight segment between two movement
-- functions would make the search space much larger.

makeNodes :: Array XY Char -> [XY]
makeNodes grid = filter isTurn $ indices grid
  where
    (_,(xmax,ymax)) = bounds grid
    isTurn xy@(x,y)
      | grid!xy == '.' = False
      | xExits + yExits == 1 = True
      | otherwise = xExits*yExits /= 0
      where
        xExits = length [() | dx <- [-1,1],
                              0 <= x+dx, x+dx <= xmax, grid!(x+dx,y) /= '.']
        yExits = length [() | dy <- [-1,1],
                              0 <= y+dy, y+dy <= ymax, grid!(x,y+dy) /= '.']

makeGraph :: Array XY Char -> [XY] -> Map XY [XY]
makeGraph grid nodes = fromList $ zip nodes (map edges nodes)
  where
    edges (x,y) = filter connected nodes
      where
        connected (x2,y2)
          | (x,y) == (x2,y2) = False
          | x == x2 = and [grid!(x,iy) /= '.' | iy <- [min y y2..max y y2]]
          | y == y2 = and [grid!(ix,y) /= '.' | ix <- [min x x2..max x x2]]
          | otherwise = False

makeStartEnd :: Array XY Char -> Map XY [XY] -> ((XY,Char),(Char,XY))
makeStartEnd grid graph = ((start,grid!start),(endDir,end))
  where
    deadEnds = map fst $ filter ((== 1) . length . snd) $ toList graph
    (start:_) = filter ((/= '#') . (grid!)) deadEnds
    (end@(endX,endY):_) = filter ((== '#') . (grid!)) deadEnds
    [(endFromX,endFromY)] = graph Data.Map.! end
    endDir
      | endFromX == endX && endFromY > endY = '^'
      | endFromX == endX && endFromY < endY = 'v'
      | endFromX > endX && endFromY == endY = '<'
      | endFromX > endX && endFromY == endY = '>'

-- Let A be the function from the start and C be the function to the end,
-- and B be used in between.
-- makeStartFunctions are possible functions for A.
-- Also for B, starting from after applying [A], [A,A], ..., [A,C], [A,C,A],
-- etc and arriving at a valid location.
makeStartFunctions :: Map XY [XY] -> Int -> (XY,Char) -> [[(Char,Int)]]
makeStartFunctions graph maxLen (startXY@(startX,startY),startDir)
  | maxLen < 4 = []
  | otherwise = map ((:[]) . fst) nextMoves ++ concatMap continue nextMoves
  where
    nextMoves :: [((Char,Int),(XY,Char))]
    nextMoves = concatMap makeNextMove $ graph Data.Map.! startXY

    continue :: ((Char,Int),(XY,Char)) -> [[(Char,Int)]]
    continue (move@(_,count),dest) =
        map (move:) $ makeStartFunctions graph
                          (maxLen - if count < 10  then 4 else 5) dest

    makeNextMove :: XY -> [((Char,Int),(XY,Char))]
    makeNextMove destXY@(destX,destY)
      | maxLen == 4 && abs (destX-startX) + abs (destY-startY) >= 10 = []
      | startX < destX && startDir == '^' = [(('R',destX-startX),(destXY,'>'))]
      | startX < destX && startDir == 'v' = [(('L',destX-startX),(destXY,'>'))]
      | startX > destX && startDir == '^' = [(('L',startX-destX),(destXY,'<'))]
      | startX > destX && startDir == 'v' = [(('R',startX-destX),(destXY,'<'))]
      | startY < destY && startDir == '>' = [(('R',destY-startY),(destXY,'v'))]
      | startY < destY && startDir == '<' = [(('L',destY-startY),(destXY,'v'))]
      | startY > destY && startDir == '>' = [(('L',startY-destY),(destXY,'^'))]
      | startY > destY && startDir == '<' = [(('R',startY-destY),(destXY,'^'))]
      | otherwise = []

makeEndFunctions :: Map XY [XY] -> Int -> [(Char,Int)] -> (Char,XY)
                 -> [[(Char,Int)]]
makeEndFunctions graph maxLen nextMoves (endDir,endXY@(endX,endY))
  | maxLen < 4 = [nextMoves]
  | null nextMoves = concatMap continue prevMoves
  | otherwise = nextMoves : concatMap continue prevMoves
  where
    prevMoves :: [((Char,Int),(Char,XY))]
    prevMoves = concatMap makePrevMove $ graph Data.Map.! endXY

    continue :: ((Char,Int),(Char,XY)) -> [[(Char,Int)]]
    continue (move@(_,count),src) =
        makeEndFunctions graph (maxLen - if count < 10 then 4 else 5)
                         (move:nextMoves) src

    makePrevMove :: XY -> [((Char,Int),(Char,XY))]
    makePrevMove srcXY@(srcX,srcY)
      | maxLen == 4 && abs (srcX-endX) + abs (srcY-endY) >= 10 = []
      | endX < srcX && endDir == '<' && srcDir 'v' =
          [(('R',srcX-endX),('v',srcXY))]
      | endX < srcX && endDir == '<' && srcDir '^' =
          [(('L',srcX-endX),('^',srcXY))]
      | endX > srcX && endDir == '>' && srcDir 'v' =
          [(('L',endX-srcX),('v',srcXY))]
      | endX > srcX && endDir == '>' && srcDir '^' =
          [(('R',endX-srcX),('^',srcXY))]
      | endY < srcY && endDir == '^' && srcDir '<' =
          [(('R',srcY-endY),('<',srcXY))]
      | endY < srcY && endDir == '^' && srcDir '>' =
          [(('L',srcY-endY),('>',srcXY))]
      | endY > srcY && endDir == 'v' && srcDir '<' =
          [(('L',endY-srcY),('<',srcXY))]
      | endY > srcY && endDir == 'v' && srcDir '>' =
          [(('R',endY-srcY),('>',srcXY))]
      | otherwise = []
      where
        srcDir 'v' = not $ null [() | (_,y) <- graph Data.Map.!srcXY, y < srcY]
        srcDir '^' = not $ null [() | (_,y) <- graph Data.Map.!srcXY, y > srcY]
        srcDir '<' = not $ null [() | (x,_) <- graph Data.Map.!srcXY, x > srcX]
        srcDir '>' = not $ null [() | (x,_) <- graph Data.Map.!srcXY, x < srcX]

functionString :: [(Char,Int)] -> String
functionString = drop 1 . concatMap f
  where f (ch,count) = ',':ch:',':show count

followF :: Map XY [XY] -> ((XY,Char),Set XY) -> [(Char,Int)]
        -> Maybe ((XY,Char),Set XY)
followF graph start [] = Just start
followF graph start@((startXY@(startX,startY),startDir),visited)
        ((turn,count):turnCounts)
  | not (member startXY graph) = Nothing
  | not (member endXY graph) = Nothing
  | otherwise =
      followF graph
              ((endXY,endDir),
               insert startXY $ insert endXY $ insert visitedXY visited)
              turnCounts
  where
    endDir
      | (startDir=='^' && turn=='R') || (startDir=='v' && turn=='L') = '>'
      | (startDir=='>' && turn=='R') || (startDir=='<' && turn=='L') = 'v'
      | (startDir=='v' && turn=='R') || (startDir=='^' && turn=='L') = '<'
      | (startDir=='<' && turn=='R') || (startDir=='>' && turn=='L') = '^'
    (endXY,visitedXY)
      | endDir == '^' = ((startX,startY-count), (startX,startY-count+1))
      | endDir == '>' = ((startX+count,startY), (startX+1,startY))
      | endDir == 'v' = ((startX,startY+count), (startX,startY+1))
      | endDir == '<' = ((startX-count,startY), (startX-count+1,startY))

followR :: Map XY [XY] -> ((XY,Char),Set XY) -> [String] -> [[(Char,Int)]]
        -> ((XY,Char),Set XY)
followR graph start [] [a,b,c] = start
followR graph start ("A":routines) fs@[a,b,c] = followR graph next routines fs
  where Just next = followF graph start a
followR graph start ("B":routines) fs@[a,b,c] = followR graph next routines fs
  where Just next = followF graph start b
followR graph start ("C":routines) fs@[a,b,c] = followR graph next routines fs
  where Just next = followF graph start c

makeRoutines :: Map XY [XY] -> ((XY,Char),(Char,XY))
             -> [([String],[[(Char,Int)]])]
makeRoutines graph (start,end@(_,endXY)) =
    concat [dfs1 1 ("A":) a c start1
            | a <- as, c <- cs,
              Just (start1,_) <- [followF graph (start,empty) a]]
  where
    as = makeStartFunctions graph 21 start
    cs = makeEndFunctions graph 21 [] end

    dfs1 rlen makeR a c start1
      | rlen > 10 = []
      | otherwise =
            maybe [] (dfs1 (rlen+1) (makeR . ("A":)) a c . fst)
                     (followF graph (start1,empty) a) ++
            maybe [] (dfs1 (rlen+1) (makeR . ("C":)) a c . fst)
                     (followF graph (start1,empty) c) ++
            concat [maybe [] (dfs2 (rlen+1) (makeR . ("B":)) a b c . fst)
                             (followF graph (start1,empty) b)
                    | b <- makeStartFunctions graph 21 start1,
                      Just (start2,_) <- [followF graph (start1,empty) b]]

    dfs2 rlen makeR a b c start2@(start2XY,_)
      | rlen > 10 = []
      | start2XY == endXY = [(makeR [],[a,b,c])]
      | otherwise =
          maybe [] (dfs2 (rlen+1) (makeR . ("A":)) a b c . fst)
                   (followF graph (start2,empty) a) ++
          maybe [] (dfs2 (rlen+1) (makeR . ("B":)) a b c . fst)
                   (followF graph (start2,empty) b) ++
          maybe [] (dfs2 (rlen+1) (makeR . ("C":)) a b c . fst)
                   (followF graph (start2,empty) c)

makeInput :: Array XY Char -> String
makeInput grid =
    unlines $ intercalate "," routine : (map functionString functions ++ ["y"])
  where
    nodes = makeNodes grid
    graph = makeGraph grid nodes
    startEnd@(start,_) = makeStartEnd grid graph
    ((routine,functions):_) = makeRoutines graph startEnd

result2 mem = last $ unsafeIntCode (map ord $ makeInput grid) (mem // [(0,2)])
  where grid = makeGrid mem
