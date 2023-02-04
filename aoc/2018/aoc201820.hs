import Data.Map(Map,alter,empty,(!))
import qualified Data.Map
import Data.Set(Set,difference,fromList,insert,size,toList,union)
import qualified Data.Set

data Regex = Path String | Choice [[Regex]] deriving Show

parse :: String -> [Regex]
parse = fst . p [] . takeWhile (/= '$') . drop 1 . dropWhile (/= '^')
  where
    p revRXs "" = (reverse revRXs,"")
    p revRXs ('(':rest) = p (choice:revRXs) afterChoice
      where (choice,afterChoice) = parseChoice [] rest
    p revRXs rest@('|':_) = (reverse revRXs,rest)
    p revRXs rest@(')':_) = (reverse revRXs,rest)
    p revRXs rest = p (Path path:revRXs) afterPath
      where (path,afterPath) = span (not . (`elem` "(|)")) rest
    parseChoice revChoices rest
      | take 1 afterChoice == "|" = parseChoice (choice:revChoices) (drop 1 afterChoice)
      | otherwise = (Choice $ reverse (choice:revChoices),drop 1 afterChoice)
      where
        (choiceOrEmpty,afterChoice) = p [] rest
        choice | null choiceOrEmpty = [Path ""] | otherwise = choiceOrEmpty

rev :: Char -> Char
rev 'N' = 'S'
rev 'S' = 'N'
rev 'E' = 'W'
rev 'W' = 'E'

move :: (Int,Int) -> Char -> (Int,Int)
move (x,y) 'N' = (x,y-1)
move (x,y) 'S' = (x,y+1)
move (x,y) 'E' = (x+1,y)
move (x,y) 'W' = (x-1,y)

mapout :: (Set (Int,Int),Map (Int,Int) (Set Char)) -> [Regex] -> (Set (Int,Int),Map (Int,Int) (Set Char))
mapout (xys,m) rxs = foldl mapout1 (xys,m) rxs

mapout1 :: (Set (Int,Int),Map (Int,Int) (Set Char)) -> Regex -> (Set (Int,Int),Map (Int,Int) (Set Char))
mapout1 (startXYs,startM) (Path path) = (fromList endXYs,endM)
  where (endXYs,endM) = foldr (mapPath path) ([],startM) (toList startXYs)
mapout1 (startXYs,startM) (Choice choices) = foldr mergeChoice (Data.Set.empty,startM) choices
  where
    mergeChoice choice (endXYs,m) = (union endXYs xys,m1)
      where (xys,m1) = mapout (startXYs,m) choice

mapPath :: String -> (Int,Int) -> ([(Int,Int)],Map (Int,Int) (Set Char)) -> ([(Int,Int)],Map (Int,Int) (Set Char))
mapPath path startXY (endXYs,startM) = (endXY:endXYs,endM)
  where
    (endXY,endM) = foldl mapPath1 (startXY,startM) path
    mapPath1 (xy,m) dir = (xy1,m2)
      where
        xy1 = move xy dir
        m1 = alter (Just . maybe (fromList [dir]) (insert dir)) xy m
        m2 = alter (Just . maybe (fromList [rev dir]) (insert (rev dir))) xy1 m1

furthestDistance :: (Int,Int) -> Map (Int,Int) (Set Char) -> Int
furthestDistance start m = search 0 (fromList [start]) (fromList [start])
  where
    search nsteps seen current
      | size current == 0 = nsteps-1
      | otherwise = search (nsteps+1) (seen `union` next) (next `difference` seen)
      where
        next = fromList (concatMap step (toList current))
        step xy = map (move xy) (toList (m!xy))

run :: String -> Int
run = furthestDistance (0,0) . snd . mapout (fromList [(0,0)],empty) . parse

test :: ()
test
  | run "^WNE$" /= 3 = error "a"
  | run "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" /= 18 = error "b"
  | run "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" /= 23 = error "c"
  | run "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" /= 31 = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap run $ readFile "input/20.txt"

furtherThan :: Int -> (Int,Int) -> Map (Int,Int) (Set Char) -> Int
furtherThan maxDist start m = search 0 (fromList [start]) (fromList [start])
  where
    search nsteps seen current
      | size current == 0 || nsteps+1 >= maxDist = Data.Map.size m-size seen
      | otherwise = search (nsteps+1) (seen `union` next) (next `difference` seen)
      where
        next = fromList (concatMap step (toList current))
        step xy = map (move xy) (toList (m!xy))

run2 :: Int -> String -> Int
run2 maxDist = furtherThan maxDist (0,0) . snd . mapout (fromList [(0,0)],empty) . parse

part2 :: IO Int
part2 = fmap (run2 1000) $ readFile "input/20.txt"
