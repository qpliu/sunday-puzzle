import Data.Map(Map,keys,member,(!))
import qualified Data.Map
import Data.Set(Set,empty,fromList,insert,intersection)

parse :: String -> ([Int],[Map Int (Int,Int)])
parse input = (draws,boards)
  where
    (drawsInput:boardsInput) = words input
    draws = map read $ words $ map (\ c -> if c == ',' then ' ' else c) drawsInput
    boards = makeBoards $ zip (map read boardsInput) (cycle [(x,y) | x <- [1..5], y <- [1..5]])
    makeBoards items
      | null items = []
      | otherwise = Data.Map.fromList next : makeBoards rest
        where (next,rest) = splitAt 25 items

playBoard :: [Int] -> Map Int (Int,Int) -> [(Int,Int)]
playBoard draws board = play 0 draws 0 empty
  where
    play nDraws [] sumHits hits = []
    play nDraws (draw:rest) sumHits hits
      | not (member draw board) = play (nDraws+1) rest sumHits hits
      | intersection hitRow nextHits == hitRow || intersection hitCol nextHits == hitCol = [(nDraws,draw*(sum (keys board) - nextSumHits))]
      | otherwise = play (nDraws+1) rest nextSumHits nextHits
      where
        nextSumHits = draw+sumHits
        nextHits = insert hit hits
        hitCol = fromList [(hitX,y) | y <- [1..5]]
        hitRow = fromList [(x,hitY) | x <- [1..5]]
        hit@(hitX,hitY) = board!draw

testData :: String
testData = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7\n"

test :: ()
test
  | (snd . minimum . concatMap (playBoard draws)) boards /= 4512 = error "a"
  | otherwise = ()
  where (draws,boards) = parse testData

part1 :: IO Int
part1 = do
    (draws,boards) <- fmap parse $ readFile "input/04.txt"
    return $ snd $ minimum $ concatMap (playBoard draws) boards

test2 :: ()
test2
  | (snd . maximum . concatMap (playBoard draws)) boards /= 1924 = error "a"
  | otherwise = ()
  where (draws,boards) = parse testData

part2 :: IO Int
part2 = do
    (draws,boards) <- fmap parse $ readFile "input/04.txt"
    return $ snd $ maximum $ concatMap (playBoard draws) boards
