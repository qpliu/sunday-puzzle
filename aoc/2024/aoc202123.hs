module AOC202123 where

import Data.Map(Map,empty,delete,fromList,insert,member,toList,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2021/input/23",
    aocTests=[
        AOCTest {
            testData=unlines [
                "#############",
                "#...........#",
                "###B#C#B#D###",
                "  #A#D#C#A#",
                "  #########"
                ],
            testResult=Just "12521",
            testResult2=Just "44169"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result,
        codeResult=result,
        codeResult2=result
        }
    }

makeHome :: Char -> [Char] -> Home
makeHome _ [] = Right 0
makeHome c cs
  | last cs == c = makeHome c (init cs)
  | otherwise = Left (length cs,zip [1..] cs)

parse input =
    (makeHome 'A' [grid!(3,2),grid!(3,3)],
     makeHome 'B' [grid!(5,2),grid!(5,3)],
     makeHome 'C' [grid!(7,2),grid!(7,3)],
     makeHome 'D' [grid!(9,2),grid!(9,3)])
  where grid = parse2d input

parse2 input =
    (makeHome 'A' [grid!(3,2),'D','D',grid!(3,3)],
     makeHome 'B' [grid!(5,2),'C','B',grid!(5,3)],
     makeHome 'C' [grid!(7,2),'B','A',grid!(7,3)],
     makeHome 'D' [grid!(9,2),'A','C',grid!(9,3)])
  where grid = parse2d input

homeX :: Char -> Int
homeX 'A' = 3
homeX 'B' = 5
homeX 'C' = 7
homeX 'D' = 9

moveCost :: Char -> Int
moveCost 'A' = 1
moveCost 'B' = 10
moveCost 'C' = 100
moveCost 'D' = 1000

type Path = (Int,State)
type Home = Either (Int,[(Int,Char)]) Int
type State = (Map Int Char,(Home,Home,Home,Home))

result :: (Home,Home,Home,Home) -> Int
result initialHomes = finalCost
  where
    Just (finalCost,_) =
        astar heuristic neighbors toState done [(0,(empty,initialHomes))]

toState :: Path -> State
toState = snd

done :: Path -> Bool
done (_,(_,(Right 0,Right 0,Right 0,Right 0))) = True
done _ = False

heuristic :: Path -> Int
heuristic (cost,(hall,(homeA,homeB,homeC,homeD))) =
    cost + heuristicHome 3 homeA + heuristicHome 5 homeB
         + heuristicHome 7 homeC + heuristicHome 9 homeD
         + heuristicHall hall
  where
    heuristicHome :: Int -> Home -> Int
    heuristicHome startX home = either (sum . map toHome . snd) (const 0) home
      where toHome (n,c) = (n + abs (homeX c - startX) + 1)*moveCost c

    heuristicHall :: Map Int Char -> Int
    heuristicHall hall = sum $ map toHome $ toList hall
      where toHome (x,c) = (abs (homeX c - x) + 1)*moveCost c

neighbors :: Path -> [Path]
neighbors path@(_,(hall,_)) =
    concatMap (moveHome path) (toList hall)
        ++ moveOutA path ++ moveOutB path ++ moveOutC path ++ moveOutD path

moveHome :: Path -> (Int,Char) -> [Path]
moveHome path (x,'A') = moveHomeA path x
moveHome path (x,'B') = moveHomeB path x
moveHome path (x,'C') = moveHomeC path x
moveHome path (x,'D') = moveHomeD path x

moveHomeA :: Path -> Int -> [Path]
moveHomeA (cost,(hall,(Right n,homeB,homeC,homeD))) x
  | x < 3 && not (or [member xx hall | xx <- [x+1..2]]) =
      [(cost + moveCost 'A'*(n+3-x),
        (delete x hall,(Right (n-1),homeB,homeC,homeD)))]
  | x > 3 && not (or [member xx hall | xx <- [4..x-1]]) =
      [(cost + moveCost 'A'*(n+x-3),
        (delete x hall,(Right (n-1),homeB,homeC,homeD)))]
moveHomeA _ _ = []

moveHomeB :: Path -> Int -> [Path]
moveHomeB (cost,(hall,(homeA,Right n,homeC,homeD))) x
  | x < 5 && not (or [member xx hall | xx <- [x+1..4]]) =
      [(cost + moveCost 'B'*(n+5-x),
        (delete x hall,(homeA,Right (n-1),homeC,homeD)))]
  | x > 5 && not (or [member xx hall | xx <- [6..x-1]]) =
      [(cost + moveCost 'B'*(n+x-5),
        (delete x hall,(homeA,Right (n-1),homeC,homeD)))]
moveHomeB _ _ = []

moveHomeC :: Path -> Int -> [Path]
moveHomeC (cost,(hall,(homeA,homeB,Right n,homeD))) x
  | x < 7 && not (or [member xx hall | xx <- [x+1..6]]) =
      [(cost + moveCost 'C'*(n+7-x),
        (delete x hall,(homeA,homeB,Right (n-1),homeD)))]
  | x > 7 && not (or [member xx hall | xx <- [8..x-1]]) =
      [(cost + moveCost 'C'*(n+x-7),
        (delete x hall,(homeA,homeB,Right (n-1),homeD)))]
moveHomeC _ _ = []

moveHomeD :: Path -> Int -> [Path]
moveHomeD (cost,(hall,(homeA,homeB,homeC,Right n))) x
  | x < 9 && not (or [member xx hall | xx <- [x+1..8]]) =
      [(cost + moveCost 'D'*(n+9-x),
        (delete x hall,(homeA,homeB,homeC,Right (n-1))))]
  | x > 9 && not (or [member xx hall | xx <- [10..x-1]]) =
      [(cost + moveCost 'D'*(n+x-9),
        (delete x hall,(homeA,homeB,homeC,Right (n-1))))]
moveHomeD _ _ = []

moveOutA :: Path -> [Path]
moveOutA (cost,(hall,(Left (homeSize,((n,c):rest)),homeB,homeC,homeD)))
  | c /= 'A' && not (null toHome) = toHome
  | otherwise = toHall nextPath c 3 succ ++ toHall nextPath c 3 pred
  where
    nextHomeA | null rest = Right homeSize | otherwise = Left (homeSize,rest)
    nextPath = (cost+n*moveCost c,(hall,(nextHomeA,homeB,homeC,homeD)))
    toHome = moveHome nextPath (3,c)
moveOutA _ = []

moveOutB :: Path -> [Path]
moveOutB (cost,(hall,(homeA,Left (homeSize,((n,c):rest)),homeC,homeD)))
  | c /= 'B' && not (null toHome) = toHome
  | otherwise = toHall nextPath c 5 succ ++ toHall nextPath c 5 pred
  where
    nextHomeB | null rest = Right homeSize | otherwise = Left (homeSize,rest)
    nextPath = (cost+n*moveCost c,(hall,(homeA,nextHomeB,homeC,homeD)))
    toHome = moveHome nextPath (5,c)
moveOutB _ = []

moveOutC :: Path -> [Path]
moveOutC (cost,(hall,(homeA,homeB,Left (homeSize,((n,c):rest)),homeD)))
  | c /= 'C' && not (null toHome) = toHome
  | otherwise = toHall nextPath c 7 succ ++ toHall nextPath c 7 pred
  where
    nextHomeC | null rest = Right homeSize | otherwise = Left (homeSize,rest)
    nextPath = (cost+n*moveCost c,(hall,(homeA,homeB,nextHomeC,homeD)))
    toHome = moveHome nextPath (7,c)
moveOutC _ = []

moveOutD :: Path -> [Path]
moveOutD (cost,(hall,(homeA,homeB,homeC,Left (homeSize,((n,c):rest)))))
  | c /= 'D' && not (null toHome) = toHome
  | otherwise = toHall nextPath c 9 succ ++ toHall nextPath c 9 pred
  where
    nextHomeD | null rest = Right homeSize | otherwise = Left (homeSize,rest)
    nextPath = (cost+n*moveCost c,(hall,(homeA,homeB,homeC,nextHomeD)))
    toHome = moveHome nextPath (9,c)
moveOutD _ = []

toHall :: Path -> Char -> Int -> (Int -> Int) -> [Path]
toHall (cost,(hall,homes)) c x nextX
  | x < 1 || x > 11 || member x hall = []
  | x == 3 || x == 5 || x == 7 || x == 9 =
                      toHall (cost+moveCost c,(hall,homes)) c (nextX x) nextX
  | otherwise = (cost,(insert x c hall,homes))
                    : toHall (cost+moveCost c,(hall,homes)) c (nextX x) nextX
