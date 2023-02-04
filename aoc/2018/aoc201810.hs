-- Iterate through the seconds as long as all the points fit in smaller and
-- smaller rectangles.

import Data.Char(isDigit)
import Data.Set(fromList,member)

parse :: String -> [((Int,Int),(Int,Int))]
parse = toPoints . toNumbers . dropWhile (not . inNum)
  where
    inNum char = isDigit char || char == '-'
    toNumbers "" = []
    toNumbers s = read n : toNumbers (dropWhile (not . inNum) rest)
      where (n,rest) = span inNum s
    toPoints (x:y:vx:vy:rest) = ((x,y),(vx,vy)) : toPoints rest
    toPoints _ = []

forward :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
forward ((x,y),v@(vx,vy)) = ((x+vx,y+vy),v)

rewind :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
rewind ((x,y),v@(vx,vy)) = ((x-vx,y-vy),v)

rect :: [((Int,Int),(Int,Int))] -> ((Int,Int),(Int,Int))
rect points = ((minimum $ map (fst . fst) points,minimum $ map (snd . fst) points),(maximum $ map (fst . fst) points,maximum $ map (snd . fst) points))

area :: ((Int,Int),(Int,Int)) -> Int
area ((x1,y1),(x2,y2)) = (x2-x1)*(y2-y1)

display :: [((Int,Int),(Int,Int))] -> String
display pointList = unlines [[if member (x,y) points then '#' else '.' | x <- [x1..x2]] | y <- [y1..y2]]
  where
    points = fromList (map fst pointList)
    ((x1,y1),(x2,y2)) = rect pointList

findMessage :: [((Int,Int),(Int,Int))] -> [((Int,Int),(Int,Int))]
findMessage points = search (area $ rect points) (map forward points)
  where
    search lastArea currentPoints
      | lastArea < (area $ rect currentPoints) = map rewind currentPoints
      | otherwise = search (area $ rect currentPoints) (map forward currentPoints)

testData :: String
testData = "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>"

test :: ()
test
  | display (findMessage $ parse testData) /= "#...#..###\n#...#...#.\n#...#...#.\n#####...#.\n#...#...#.\n#...#...#.\n#...#...#.\n#...#..###\n" = error "a"
  | otherwise = ()

part1 :: IO ()
part1 = readFile "input/10.txt" >>= putStrLn . display . findMessage . parse

findMessage2 :: [((Int,Int),(Int,Int))] -> (Int,[((Int,Int),(Int,Int))])
findMessage2 points = search 1 (area $ rect points) (map forward points)
  where
    search t lastArea currentPoints
      | lastArea < (area $ rect currentPoints) = (t-1,map rewind currentPoints)
      | otherwise = search (t+1) (area $ rect currentPoints) (map forward currentPoints)

test2 :: ()
test2
  | display (snd $ findMessage2 $ parse testData) /= "#...#..###\n#...#...#.\n#...#...#.\n#####...#.\n#...#...#.\n#...#...#.\n#...#...#.\n#...#..###\n" = error "a"
  | fst (findMessage2 $ parse testData) /= 3 = error "b"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (fst . findMessage2 . parse) $ readFile "input/10.txt"
