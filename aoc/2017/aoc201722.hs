import Data.Map(Map,findWithDefault)
import qualified Data.Map
import Data.Set(Set,delete,fromList,insert,member,toList)

parse :: String -> (Int,((Int,Int),(Int,Int),Set (Int,Int)))
parse s = p s 0 0 0 0 []
  where
    p ('.':chars) x y xmax ymax points = p chars (x+1) y (max x xmax) (max y ymax) points
    p ('#':chars) x y xmax ymax points = p chars (x+1) y (max x xmax) (max y ymax) ((x,y):points)
    p ('\n':chars) x y xmax ymax points = p chars 0 (y+1) xmax ymax points
    p (_:chars) x y xmax ymax points = p chars x y xmax ymax points
    p "" _ _ xmax ymax points = (0,((xmax `div` 2,ymax `div` 2),(0,-1),fromList points))

burst :: (Int,((Int,Int),(Int,Int),Set (Int,Int))) -> (Int,((Int,Int),(Int,Int),Set (Int,Int)))
burst (infectCount,((x,y),(dx,dy),grid))
  | (x,y) `member` grid = (infectCount,((x-dy,y+dx),(-dy,dx),delete (x,y) grid))
  | otherwise = (infectCount+1,((x+dy,y-dx),(dy,-dx),insert (x,y) grid))

testData :: String
testData = "..#\n#..\n..."

test :: ()
test
  | fst (head $ drop 7 $ iterate burst $ parse testData) /= 5 = error "a"
  | fst (head $ drop 70 $ iterate burst $ parse testData) /= 41 = error "b"
  | fst (head $ drop 10000 $ iterate burst $ parse testData) /= 5587 = error "c"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (fst . head . drop 10000 . iterate burst . parse) $ readFile "input/22.txt"

data State = Clean | Weakened | Infected | Flagged deriving Eq

parse2 :: String -> (Int,((Int,Int),(Int,Int),Map (Int,Int) State))
parse2 s = (infectCount,((x,y),(dx,dy),convert grid))
  where
    (infectCount,((x,y),(dx,dy),grid)) = parse s
    convert grid = Data.Map.fromList [((x,y),Infected) | (x,y) <- toList grid]

burst2 :: (Int,((Int,Int),(Int,Int),Map (Int,Int) State)) -> (Int,((Int,Int),(Int,Int),Map (Int,Int) State))
burst2 (infectCount,((x,y),(dx,dy),grid)) = b (findWithDefault Clean (x,y) grid)
  where
    b Clean = (infectCount,((x+dy,y-dx),(dy,-dx),Data.Map.insert (x,y) Weakened grid))
    b Weakened = (infectCount+1,((x+dx,y+dy),(dx,dy),Data.Map.insert (x,y) Infected grid))
    b Infected = (infectCount,((x-dy,y+dx),(-dy,dx),Data.Map.insert (x,y) Flagged grid))
    b Flagged = (infectCount,((x-dx,y-dy),(-dx,-dy),Data.Map.delete (x,y) grid))

test2 :: ()
test2
  | fst (head $ drop 100 $ iterate burst2 $ parse2 testData) /= 26 = error "a"
  | fst (head $ drop 10000000 $ iterate burst2 $ parse2 testData) /= 2511944 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (fst . head . drop 10000000 . iterate burst2 . parse2) $ readFile "input/22.txt"
