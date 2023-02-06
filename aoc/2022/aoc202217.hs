import Data.Set(Set,fromList,intersection,size,union)
import qualified Data.Set

type Chamber = Set (Int,Int) -- (y,x) to make finding maximum easier
type Rock = (Int,Set (Int,Int))

rockList :: [Rock]
rockList = [
    (4,fromList [(0,0),(0,1),(0,2),(0,3)]),
    (3,fromList [(1,0),(2,1),(1,1),(0,1),(1,2)]),
    (3,fromList [(0,0),(0,1),(0,2),(1,2),(2,2)]),
    (1,fromList [(0,0),(1,0),(2,0),(3,0)]),
    (2,fromList [(0,0),(1,0),(0,1),(1,1)])
    ]

translate :: Rock -> (Int,Int) -> Set (Int,Int)
translate (_,points) (y,x) = Data.Set.map xl points
  where xl (yy,xx) = (yy+y,xx+x)

initChamber :: Int -> Chamber
initChamber width = fromList [(0,x) | x <- [0..width-1]]

nextRock :: Int -> (String,Chamber) -> Rock -> (String,Chamber)
nextRock width (jets,chamber) rock = 
    push width (jets,chamber) (4+fst (maximum chamber),2) rock

push :: Int -> (String,Chamber) -> (Int,Int) -> Rock -> (String,Chamber)
push width ((jet:jets),chamber) (y,x) rock@(w,_)
  | jet == '<' && x <= 0 = fall width (jets,chamber) (y,x) rock
  | jet == '>' && x+w >= width = fall width (jets,chamber) (y,x) rock
  | jet == '<' && size (intersection chamber (translate rock (y,x-1))) /= 0 =
      fall width (jets,chamber) (y,x) rock
  | jet == '>' && size (intersection chamber (translate rock (y,x+1))) /= 0 =
      fall width (jets,chamber) (y,x) rock
  | jet == '<' = fall width (jets,chamber) (y,x-1) rock
  | otherwise = fall width (jets,chamber) (y,x+1) rock

fall :: Int -> (String,Chamber) -> (Int,Int) -> Rock -> (String,Chamber)
fall width (jets,chamber) (y,x) rock
  | size (intersection chamber (translate rock (y-1,x))) /= 0 =
      (jets,union chamber (translate rock (y,x)))
  | otherwise = push width (jets,chamber) (y-1,x) rock

run1 :: Int -> Int -> String -> Chamber
run1 width rockCount jets =
    snd $ foldl (nextRock width) (cycle (filter (`elem` "<>") jets),initChamber width) (take rockCount (cycle rockList))

testData :: String
testData = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>\n"

test :: ()
test
  | (fst . maximum . run1 7 2022) testData /= 3068 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (fst . maximum . run1 7 2022) $ readFile "input/17.txt"

-- Some multiple of 5 and the length of the jet pattern should result in
-- a repeating pattern.

-- In the example, the jet pattern has a length 40, and the tower grows
-- by 424 every 40*7 rocks.  The first 120 rocks makes the tower height 184.
-- after that, every 40*7 rocks makes the tower higher by 424.
-- 120+40*7*3571428571 = 1000000000000
-- 184 + 424*3571428571 = 1514285714288

-- My input data has length 10091, so a repeating pattern probably has a length
-- that is a multiple of 5*10091 = 50455.

-- My input data eventually results in a tower that increases in height
-- by 85157 every 10881*5 rocks.
-- 29485 + 18380663*(10881*5) = 1000000000000
-- After 29485 rocks, the height is 46110.
-- 46110 + 18380663*85157 = 1565242165201.

run2 :: Int -> String -> [(Int,Int)]
run2 width input = doCycle 0 (cycle jets,initChamber width)
  where
    jets = filter (`elem` "<>") input
    n = length rockList -- *length jets `div` (gcd (length rockList) (length jets))
    rocks = take n (cycle rockList)

    doCycle rockCount (curJets,chamber) = (rockCount,fst $ maximum chamber) : doCycle (rockCount+n) (foldl (nextRock width) (curJets,chamber) rocks)
