module AOC202222 where

import Data.Char(isDigit)
import Data.Map(Map,elems,empty,fromList,keys,mapWithKey,mapKeys,member,toList,(!))
import qualified Data.Map
import Data.Tuple(swap)

import AOC

aoc = AOC {
    day="../../2022/input/22",
    aocTests=[
        AOCTest {
            testData=unlines [
                "        ...#",
                "        .#..",
                "        #...",
                "        ....",
                "...#.......#",
                "........#...",
                "..#....#....",
                "..........#.",
                "        ...#....",
                "        .....#..",
                "        .#......",
                "        ......#.",
                "",
                "10R5L5R10L4R5L5"
                ],
            testResult=Just "6032",
            testResult2=Just "5031"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2 4,
        codeResult=result,
        codeResult2=result2 50
        }
    }

-- The maximum number in the path in my data is 50, so it's not worth it
-- to detect if moving forward a large number of tiles from some tile
-- would loop through that tile multiple times.

type XY = (Int,Int)

-- Add an R to the start of the path, so the starting direction is Up.
parse :: String -> ((Dir,XY),[(Char,Int)],Map XY Bool)
parse input = ((Up,start),path ('R':p),board)
  where
    (m,["",p]) = span (not . null) $ lines input
    board =
        Data.Map.map (/= '#') $ Data.Map.filter (/= ' ') $ parse2d $ unlines m
    path (turn:inp)
      | null rest = [(turn,read n)]
      | otherwise = (turn,read n) : path rest
      where (n,rest) = span isDigit inp
    start = minimum $ filter ((== 0) . snd) $ keys board

data Dir = Rt | Dn | Lt | Up deriving (Bounded,Enum,Eq,Ord,Show)

facing :: Dir -> Int
facing = fromEnum

turnR :: Dir -> Dir
turnR = toEnum . (`mod` 4) . (+1) . fromEnum

turnL :: Dir -> Dir
turnL = toEnum . (`mod` 4) . (+3) . fromEnum

toStops :: Map XY Bool -> Map XY (Map Dir (XY,Int))
toStops board = stops
  where
    stops = mapWithKey getToStops board
    getToStops xy@(x,y) notWall
      | not notWall = empty
      | otherwise = fromList [(Rt,toRt),(Dn,toDn),(Lt,toLt),(Up,toUp)]
      where
        toRt
          | member (x+1,y) board && board!(x+1,y) =
              fmap (+1) $ stops!(x+1,y)!Rt
          | otherwise = (xy,0)
        toDn
          | member (x,y+1) board && board!(x,y+1) =
              fmap (+1) $ stops!(x,y+1)!Dn
          | otherwise = (xy,0)
        toLt
          | member (x-1,y) board && board!(x-1,y) =
              fmap (+1) $ stops!(x-1,y)!Lt
          | otherwise = (xy,0)
        toUp
          | member (x,y-1) board && board!(x,y-1) =
              fmap (+1) $ stops!(x,y-1)!Up
          | otherwise = (xy,0)

wrapAround :: ((Dir,XY) -> (Dir,XY))
           -> Map XY (Map Dir (XY,Int))
           -> Map XY (Map Dir (Either (XY,Int) (Dir,(XY,Int))))
wrapAround getWrappedXY stops = mapWithKey getWrapAround stops
  where
    getWrapAround xy xyStops = mapWithKey wrap xyStops
      where
        wrap dir stop
          | member (move dir xy) stops = Left stop
          | member wrappedDir (stops!wrappedXY) =
              Right (wrappedDir,fmap (+1) (stops!wrappedXY!wrappedDir))
          | otherwise = Left stop
          where (wrappedDir,wrappedXY) = getWrappedXY (dir,xy)
        move Rt (x,y) = (x+1,y)
        move Dn (x,y) = (x,y+1)
        move Lt (x,y) = (x-1,y)
        move Up (x,y) = (x,y-1)

wrapTorus :: Map XY a -> (Dir,XY) -> (Dir,XY)
wrapTorus board (dir,(x,y))
  | dir == Rt = (dir,wrap (-1,0) (x,y))
  | dir == Dn = (dir,wrap (0,-1) (x,y))
  | dir == Lt = (dir,wrap (1,0) (x,y))
  | dir == Up = (dir,wrap (0,1) (x,y))
  where
    wrap (dx,dy) (x,y)
      | member (x+dx,y+dy) board = wrap (dx,dy) (x+dx,y+dy)
      | otherwise = (x,y)

followPath :: Map XY (Map Dir (Either (XY,Int) (Dir,(XY,Int))))
           -> (Dir,XY) -> (Char,Int) -> (Dir,XY)
followPath wrappedBoard (startDir,startXY) (turn,count) = end
  where
    moveDir | turn == 'R' = turnR startDir | otherwise = turnL startDir
    end = move (moveDir,startXY) count
    move :: (Dir,XY) -> Int -> (Dir,XY)
    move (dir,xy) n =
        either (toStop n dir) (wrapToStop n) (wrappedBoard!xy!dir)
    toStop :: Int -> Dir -> (XY,Int) -> (Dir,XY)
    toStop n dir (dest,dist)
      | n > dist = continueMove (dir,dest) (n-dist)
      | otherwise = backup dir dest (dist-n)
    wrapToStop :: Int -> (Dir,(XY,Int)) -> (Dir,XY)
    wrapToStop n (dir,(dest,dist))
      | n > dist = continueMove (dir,dest) (n-dist)
      | otherwise = backup dir dest (dist-n)

    continueMove :: (Dir,XY) -> Int -> (Dir,XY)
    continueMove (dir,xy) n =
        either (toWall n dir) (toWrapAround n) (wrappedBoard!xy!dir)
    toWall :: Int -> Dir -> (XY,Int) -> (Dir,XY)
    toWall n dir (dest,dist)
      | n >= dist = (dir,dest)
      | otherwise = backup dir dest (dist-n)
    toWrapAround :: Int -> (Dir,(XY,Int)) -> (Dir,XY)
    toWrapAround n (dir,(dest,dist))
      | n > dist = continueMove (dir,dest) (n-dist)
      | otherwise = backup dir dest (dist-n)
    backup :: Dir -> XY -> Int -> (Dir,XY)
    backup dir (x,y) n
      | dir == Rt = (dir,(x-n,y))
      | dir == Dn = (dir,(x,y-n))
      | dir == Lt = (dir,(x+n,y))
      | dir == Up = (dir,(x,y+n))

password :: (Dir,XY) -> Int
password (dir,(x,y)) = facing dir + 4*(x+1) + 1000*(y+1)

result (start,path,board) =
    password $ foldl (followPath wrappedBoard) start path
  where
    wrappedBoard = wrapAround (wrapTorus board) $ toStops board

type XYZ = (Int,Int,Int)

toCube :: Int -> Map XY a -> Map XY XYZ
toCube sideLength board = cube
  where
    s = sideLength

    -- Test data is
    --    B
    --  NWS
    --    TE

    -- My input is
    --   BE
    --   S
    --  WT
    --  N

    -- Of the faces on the northernmost row, choose the one furthest west.
    -- That face will be the bottom face, and can only be attached to
    -- faces to the east and/or to the south.
    xmin
      | member (0,0) board = 0
      | member (s,0) board = s
      | member (2*s,0) board = 2*s
      | member (3*s,0) board = 3*s

    cubeB = fromList $ map addZ $ keys board
      where addZ (x,y) = ((x,y),(x-xmin+1,y+1,0))

    -- The possible boards are
    --                B---E---T-- can't go W -- N would be cut off
    --                |   |   |
    --                |   |   SWN
    --                |   |
    --                |   |        W
    --                |   S-----T--N
    --                |   |    NW
    --                |   WT
    --                |   N
    --                |
    --  E--N--W-------S-------E--N--W
    --  T ET NT       |       TN TW T
    --        E     W-T-E     W
    --              N | N
    --              E | W
    --                |
    --               WNE

    cube = foldBS $ foldBE cubeB

    foldBE c
      | not (elem (s+1,1,0) (elems c)) = c -- path to E goes through S
      | otherwise = foldBES $ foldBET $ Data.Map.map foldup c
      where
        foldup (x,y,z) | x >= s+1 && z == 0 = (s+1,y,1+(x-(s+1)))
        foldup xyz = xyz

    foldBET c
      | not (elem (s+1,1,s+1) (elems c)) = c -- path to T goes through S
      | otherwise = foldBETS $ Data.Map.map foldup c
      where
        foldup (x,y,z) | z >= s+1 && x == s+1 = (s-(z-(s+1)),y,s+1)
        foldup xyz = xyz

    foldBETS c
      | otherwise = foldBETSW $ Data.Map.map foldup c
      where
        foldup (x,y,z) | y >= s+1 && z == s+1 = (x,s+1,s-(y-(s+1)))
        foldup xyz = xyz

    foldBETSW c
      | otherwise = foldBETSWN $ Data.Map.map foldup c
      where
        foldup (x,y,z) | x <= 0 && y == s+1 && z > 0 = (0,s+x,z)
        foldup xyz = xyz

    foldBETSWN c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | x == 0 && y <= 0 && z > 0 = (1-y,0,z)
        foldup xyz = xyz

    foldBES c
      | not (elem (s+1,s+1,1) (elems c)) = c
      | otherwise = foldBESW $ foldBEST $ Data.Map.map foldup c
      where
        foldup (x,y,z) | y >= s+1 && x == s+1 && z > 0 = (s+1,s-(y-(s+1)),z)
        foldup xyz = xyz

    foldBEST c
      | not (elem (1,s+1,s+1) (elems c)) = c
      | otherwise = foldBESTW $ foldBESTN $ Data.Map.map foldup c
      where
        foldup (x,y,z) | z >= s+1 && y == s+1 = (x,s-(z-(s+1)),s+1)
        foldup xyz = xyz

    foldBESTN c
      | otherwise = foldBESTNW $ Data.Map.map foldup c
      where
        foldup (x,y,z) | y <= 0 && z == s+1 = (x,0,s+y)
        foldup xyz = xyz

    foldBESTNW c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | x <= 0 && y == 0 && z > 0 = (0,1-x,z)
        foldup xyz = xyz

    foldBESTW c
      | otherwise = foldBESTWN $ Data.Map.map foldup c
      where
        foldup (x,y,z) | x <= 0 && z == s+1 = (0,y,s+x)
        foldup xyz = xyz

    foldBESTWN c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | y <= 0 && x == 0 && z > 0 = (1-y,0,z)
        foldup xyz = xyz

    foldBESW c
      | otherwise = foldBESWN $ foldBESWT $ Data.Map.map foldup c
      where
        foldup (x,y,z) | x <= 0 && y == s+1 && z > 0 = (0,s+x,z)
        foldup xyz = xyz

    foldBESWN c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | y <= 0 && x == 0 && z > 0 = (1-y,0,z)
        foldup xyz = xyz

    foldBESWT c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | z >= s+1 && x == 0 = (1+(z-(s+1)),y,s+1)
        foldup xyz = xyz

    foldBS c
      | not (elem (1,s+1,0) (elems c)) = c
      | otherwise = foldBST $ foldBSW $ foldBSE $ Data.Map.map foldup c
      where
        foldup (x,y,z) | y >= s+1 && z == 0 = (x,s+1,1+(y-(s+1)))
        foldup xyz = xyz

    foldBSE c
      | not (elem (s+1,s+1,1) (elems c)) = c -- path to E goes through T
      | otherwise = foldBSET $ foldBSEN $ Data.Map.map foldup c
      where
        foldup (x,y,z) | x >= s+1 && y == s+1 = (s+1,s-(x-(s+1)),z)
        foldup xyz = xyz

    foldBSET c
      | otherwise = foldBSETN $ foldBSETW $ Data.Map.map foldup c
      where
        foldup (x,y,z) | x == s+1 && z >= s+1 && y < s+1 = (s-(z-(s+1)),y,s+1)
        foldup xyz = xyz

    foldBSETN c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | y <= 0 && z == s+1 = (x,0,s+y)
        foldup xyz = xyz

    foldBSETW c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | x <= 0 && z == s+1 && y < s+1 = (0,y,s+x)
        foldup xyz = xyz

    foldBSEN c
      | otherwise = foldBSENT $ foldBSENW $ Data.Map.map foldup c
      where
        foldup (x,y,z) | y <= 0 && x == s+1 = (s+y,0,z)
        foldup xyz = xyz

    foldBSENW c
      | otherwise = foldBSENWT $ Data.Map.map foldup c
      where
        foldup (x,y,z) | x <= 0 && y == 0 = (0,1-x,z)
        foldup xyz = xyz

    foldBSENWT c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | z >= s+1 && x == 0 && y < s+1 = (1+(z-(s+1)),y,s+1)
        foldup xyz = xyz

    foldBSENT c
      | otherwise = foldBSENTW $ Data.Map.map foldup c
      where
        foldup (x,y,z) | z >= s+1 && y == 0 = (x,1+(z-(s+1)),s+1)
        foldup xyz = xyz

    foldBSENTW c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | x <= 0 && z == s+1 && y < s+1 = (0,y,s+x)
        foldup xyz = xyz

    foldBSW c
      | not (elem (0,s+1,1) (elems c)) = c -- path to W goes through T
      | otherwise = foldBSWT $ foldBSWN $ Data.Map.map foldup c
      where
        foldup (x,y,z) | x <= 0 && y == s+1 = (0,s+x,z)
        foldup xyz = xyz

    foldBSWN c
      | otherwise = foldBSWNE $ foldBSWNT $ Data.Map.map foldup c
      where
        foldup (x,y,z) | y <= 0 && x == 0 = (1-y,0,z)
        foldup xyz = xyz

    foldBSWNE c
      | otherwise = foldBSWNET $ Data.Map.map foldup c
      where
        foldup (x,y,z) | x >= s+1 && y == 0 = (s+1,1+(x-(s+1)),z)
        foldup xyz = xyz

    foldBSWNET c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | z >= s+1 && x == s+1 && y < s+1 = (s-(z-(s+1)),y,s+1)
        foldup xyz = xyz

    foldBSWNT c
      | otherwise = foldBSWNTE $ Data.Map.map foldup c
      where
        foldup (x,y,z) | z >= s+1 && y == 0 = (x,1+(z-(s+1)),s+1)
        foldup xyz = xyz

    foldBSWNTE c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | x >= s+1 && z == s+1 && y < s+1 = (s+1,y,s-(x-(s+1)))
        foldup xyz = xyz

    foldBSWT c
      | otherwise = foldBSWTN $ foldBSWTE $ Data.Map.map foldup c
      where
        foldup (x,y,z) | z >= s+1 && x == 0 && s < s+1 = (1+(z-(s+1)),y,s+1)
        foldup xyz = xyz

    foldBSWTN c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | y <= 0 && z == s+1 = (x,0,s+y)
        foldup xyz = xyz

    foldBSWTE c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | x >= s+1 && z == s+1 && y < s+1 = (s+1,y,s-(x-(s+1)))
        foldup xyz = xyz

    foldBST c
      | not (elem (1,s+1,s+1) (elems c)) = c
      | otherwise = foldBSTW $ foldBSTE $ foldBSTN $ Data.Map.map foldup c
      where
        foldup (x,y,z) | z >= s+1 && y == s+1 = (x,s-(z-(s+1)),s+1)
        foldup xyz = xyz

    foldBSTW c
      | not (elem (0,1,s+1) (elems c)) = c
      | otherwise = foldBSTWN $ Data.Map.map foldup c
      where
        foldup (x,y,z) | x <= 0 && z == s+1 = (0,y,s+x)
        foldup xyz = xyz

    foldBSTWN c
      | not (elem (0,0,1) (elems c)) = c
      | otherwise = foldBSTWNE $ Data.Map.map foldup c
      where
        foldup (x,y,z) | y <= 0 && x == 0 = (1-y,0,z)
        foldup xyz = xyz

    foldBSTWNE c
      | not (elem (s+1,0,1) (elems c)) = c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | x >= s+1 && y == 0 = (s+1,1+(x-(s+1)),z)
        foldup xyz = xyz

    foldBSTE c
      | not (elem (s+1,1,s+1) (elems c)) = c
      | otherwise = foldBSTEN $ Data.Map.map foldup c
      where
        foldup (x,y,z) | x >= s+1 && z == s+1 = (s+1,y,s-(x-(s+1)))
        foldup xyz = xyz

    foldBSTEN c
      | otherwise = foldBSTENW $ Data.Map.map foldup c
      where
        foldup (x,y,z) | y <= 0 && x == s+1 = (s+y,0,z)
        foldup xyz = xyz

    foldBSTENW c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | x <= 0 && y == 0 = (0,1-x,z)
        foldup xyz = xyz

    foldBSTN c
      | not (elem (1,0,s+1) (elems c)) = c
      | otherwise = foldBSTNW $ foldBSTNE $ Data.Map.map foldup c
      where
        foldup (x,y,z) | y <= 0 && z == s+1 = (x,0,s+y)
        foldup xyz = xyz

    foldBSTNW c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | x <= 0 && y == 0 = (0,1-x,z)
        foldup xyz = xyz

    foldBSTNE c
      | otherwise = Data.Map.map foldup c
      where
        foldup (x,y,z) | x >= s+1 && y == 0 = (s+1,1+(x-(s+1)),z)
        foldup xyz = xyz

wrapCube :: Int -> Map XY a -> (Dir,XY) -> (Dir,XY)
wrapCube sideLength board = (wraps!)
  where
    cube :: Map XY XYZ
    cube = toCube sideLength board
    
    fromCube :: Map XYZ XY
    fromCube = fromList $ map swap $ toList cube

    s = sideLength

    wraps = fromList $ concat
        [[(get2d (x,y2,z) (x,y1,z) True,
           get2d (x,y,z1) (x,y,z2) False),
          (get2d (x,y,z2) (x,y,z1) True,
           get2d (x,y1,z) (x,y2,z) False),

          (get2d (y2,x,z) (y1,x,z) True,
           get2d (y,x,z1) (y,x,z2) False),
          (get2d (y,x,z2) (y,x,z1) True,
           get2d (y1,x,z) (y2,x,z) False),

          (get2d (z,y2,x) (z,y1,x) True,
           get2d (z1,y,x) (z2,y,x) False),
          (get2d (z2,y,x) (z1,y,x) True,
           get2d (z,y1,x) (z,y2,x) False)
         ]
         | x <- [1..s],
           (y,y1,y2) <- [(0,1,2),(s+1,s,s-1)],
           (z,z1,z2) <- [(0,1,2),(s+1,s,s-1)]]
    get2d :: XYZ -> XYZ -> Bool -> (Dir,XY)
    get2d xyz1 xyz2 out
      | out = (dir,(x2,y2))
      | otherwise = (dir,(x1,y1))
      where
        (x1,y1) = fromCube!xyz1
        (x2,y2) = fromCube!xyz2
        dir
          | x2 > x1 = Rt
          | y2 > y1 = Dn
          | x2 < x1 = Lt
          | y2 < y1 = Up

result2 sideLength (start,path,board) =
    password $ foldl (followPath wrappedBoard) start path
  where
    wrappedBoard = wrapAround (wrapCube sideLength board) $ toStops board
