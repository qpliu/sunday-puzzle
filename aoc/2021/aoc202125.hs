import Data.Set(Set,fromList,member)
import qualified Data.Set

parse :: String -> ((Int,Int),Set (Int,Int),Set (Int,Int))
parse = p 0 0 0 0 [] []
  where
    p x y xmax ymax rights downs "" = ((xmax,ymax),fromList rights,fromList downs)
    p x y xmax ymax rights downs (c:rest)
      | c == '\n' = p 0 (y+1) xmax ymax rights downs rest
      | c == '.' = p (x+1) y (max x xmax) (max y ymax) rights downs rest
      | c == '>' = p (x+1) y (max x xmax) (max y ymax) ((x,y):rights) downs rest
      | c == 'v' = p (x+1) y (max x xmax) (max y ymax) rights ((x,y):downs) rest
      | otherwise = p (x+1) y xmax ymax rights downs rest

step :: ((Int,Int),Set (Int,Int),Set (Int,Int)) -> ((Int,Int),Set (Int,Int),Set (Int,Int))
step ((xmax,ymax),rights,downs) = ((xmax,ymax),nextRights,nextDowns)
  where
    nextRights = Data.Set.map moveRight rights
    nextDowns = Data.Set.map moveDown downs
    moveRight (x,y)
      | member (newX,y) rights || member (newX,y) downs = (x,y)
      | otherwise = (newX,y)
      where newX = (x+1) `mod` (xmax+1)
    moveDown (x,y)
      | member (x,newY) nextRights || member (x,newY) downs = (x,y)
      | otherwise = (x,newY)
      where newY = (y+1) `mod` (ymax+1)

countSteps :: Int -> ((Int,Int),Set (Int,Int),Set (Int,Int)) -> Int
countSteps n cukes
  | newCukes == cukes = n+1
  | otherwise = countSteps (n+1) newCukes
  where
    newCukes = step cukes

testData :: String
testData = "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>\n"

test :: ()
test
  | (countSteps 0 . parse) testData /= 58 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (countSteps 0 . parse) $ readFile "input/25.txt"
