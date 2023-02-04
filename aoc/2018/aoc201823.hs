import Data.Char(isDigit)

parse :: String -> [(Int,Int,Int,Int)]
parse = map p . lines
  where
    p s = (read r,read x,read y,read z)
      where
        (x,s1) = span isNum $ dropWhile (not . isNum) s
        (y,s2) = span isNum $ dropWhile (not . isNum) s1
        (z,s3) = span isNum $ dropWhile (not . isNum) s2
        r = filter isNum s3
    isNum c = isDigit c || c == '-'

inRange :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Bool
inRange (r1,x1,y1,z1) (r2,x2,y2,z2) =
    r1 >= abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

getCount :: [(Int,Int,Int,Int)] -> Int
getCount bots = length $ filter (inRange (maximum bots)) bots

testData :: String
testData = "pos=<0,0,0>, r=4\npos=<1,0,0>, r=1\npos=<4,0,0>, r=3\npos=<0,2,0>, r=1\npos=<0,5,0>, r=3\npos=<0,0,3>, r=1\npos=<1,1,1>, r=1\npos=<1,1,2>, r=1\npos=<1,3,1>, r=1"

test :: ()
test
  | getCount (parse testData) /= 7 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (getCount . parse) $ readFile "input/23.txt"

testData2 :: String
testData2 = "pos=<10,12,12>, r=2\npos=<12,14,12>, r=2\npos=<16,12,12>, r=4\npos=<14,14,14>, r=6\npos=<50,50,50>, r=200\npos=<10,10,10>, r=5"

-- First, get a rough idea of where the answer might be.
rescale :: Int -> [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
rescale factor = map divide
  where divide (r,x,y,z) = (r `div` factor,x `div` factor,y `div` factor,z `div` factor)

bruteForce :: Int -> Int -> (Int,Int,Int) -> [(Int,Int,Int,Int)] -> [(Int,(Int,(Int,Int,Int)))]
bruteForce maxDist limit (x0,y0,z0) bots = [(0 - length (filter (`inRange` (0,x,y,z)) bots),(abs x + abs y + abs z,(x,y,z))) | x <- [x0-limit..x0+limit], y <- [y0-limit..y0+limit], z <- [z0-limit..z0+limit], abs x + abs y + abs z < maxDist]

-- Scaling out my input data, the largest number of bots in range is 879.
-- The minimum distance from (0,0,0) is 95.5M - 95.7M.

-- Zooming in, the largest number of bots is 894.
-- The minimum distance is 95.5409M - 95.5411M.

-- Zooming in more, 907 bots, minimum distance 95541.00k - 95541.02k.

-- Fully zoomed, 910 bots, minimum distance 95541011.

-- This code keeps going down the wrong path.
search :: (Int,Int,[(Int,Int,Int,Int)]) -> (Int,Int,(Int,(Int,Int,Int))) -> (Int,Int,(Int,(Int,Int,Int)))
search (f,limit,bots) (nbots,scaleFactor,(dist,(x,y,z))) = (-negBots,sf,(newDist*sf,(newx*sf,newy*sf,newz*sf)))
  where
    sf = scaleFactor `div` f
    (negBots,(newDist,(newx,newy,newz))) = minimum $ bruteForce (dist`div`sf+2) limit (x`div`sf,y`div`sf,z`div`sf) (rescale sf bots)
