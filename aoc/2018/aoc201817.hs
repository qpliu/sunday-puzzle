import Data.Char(isDigit)
import Data.Map(Map,elems,findWithDefault,fromList,insert)

type Ground = ((Int,Int),Map (Int,Int) Char)

parse :: String -> Ground
parse s = ((minimum (map snd clay),maximum (map snd clay)),fromList (zip clay (repeat '#')))
  where
    clay = concatMap parseVein (lines s)
    num = read . filter isDigit
    parseVein s
      | head s == 'x' = [(num n1,y) | y <- [num n2 .. num n3]]
      | otherwise = [(x,num n1) | x <- [num n2 .. num n3]]
      where
        (n1,s2) = span (/= ',') s
        (n2,n3) = span (/= '.') s2

l :: (Int,Int) -> (Int,Int)
l (x,y) = (x-1,y)

r :: (Int,Int) -> (Int,Int)
r (x,y) = (x+1,y)

u :: (Int,Int) -> (Int,Int)
u (x,y) = (x,y-1)

d :: (Int,Int) -> (Int,Int)
d (x,y) = (x,y+1)

look :: Ground -> (Int,Int) -> Char
look ((ymin,ymax),grid) (x,y)
  | y < ymin || y > ymax = '.'
  | otherwise = findWithDefault '.' (x,y) grid

set :: Ground -> (Int,Int) -> Char -> Ground
set ground@((ymin,ymax),grid) (x,y) char
  | y < ymin || y > ymax = ground
  | otherwise = ((ymin,ymax),insert (x,y) char grid)

flow :: (Int,Int) -> Ground -> Either Ground Ground
flow xy@(x,y) ground@((ymin,ymax),grid)
  | y < ymin = flow (x,ymin) ground
  | y > ymax = Left ground
  | look ground xy == '.' && y == ymax = Right (set ground xy '|')
  | look ground xy == '.' = flow xy (set ground xy '|')
  | look ground xy /= '|' = error (show (xy,look ground xy))
  | look ground (d xy) `elem` ".|" = flow (d xy) ground
  | leftContained && rightContained = Right (fill (fill ground l xy '~') r xy '~')
  | look ground (l xy) == '.' = Right (fill ground l xy '|')
  | look ground (r xy) == '.' = Right (fill ground r xy '|')
  | otherwise = maybe (maybe (Left ground) Right flowRight) Right flowLeft
  where
    leftContained = contained l xy
    rightContained = contained r xy
    flowLeft = flowSide l xy
    flowRight = flowSide r xy

    fill ground dir xy char
      | look ground xy == '#' = ground
      | look ground (d xy) `elem` ".|" = set ground xy char
      | otherwise = fill (set ground xy char) dir (dir xy) char

    contained dir xy
      | look ground xy == '#' = True
      | look ground (d xy) `elem` ".|" = False
      | otherwise = contained dir (dir xy)

    flowSide dir xy
      | look ground xy == '#' = Nothing
      | look ground (d xy) `elem` ".|" = either (const Nothing) Just (flow (d xy) ground)
      | otherwise = flowSide dir (dir xy)

spring :: (Int,Int) -> Ground -> Ground
spring source ground = either id (spring source) (flow source ground)

countWater :: Ground -> Int
countWater (_,grid) = length $ filter (`elem` "~|") $ elems grid

testData :: String
testData = "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504"

test :: ()
test
  | countWater (spring (500,0) $ parse testData) /= 57 = error "a"
  | otherwise = ()

-- This is too slow.  The Go code is slow too, but eventually gets the answer.
part1 :: IO Int
part1 = fmap (countWater . spring (500,0) . parse) $ readFile "input/17.txt"
