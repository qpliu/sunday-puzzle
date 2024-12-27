module AOC202415 where

import Data.Array(assocs,(!))
import Data.Set(Set,delete,difference,elems,fromList,insert,member,union)

import AOC

aoc = AOC {
    day="15",
    aocTests=[
        AOCTest {
            testData=unlines [
                "##########",
                "#..O..O.O#",
                "#......O.#",
                "#.OO..O.O#",
                "#..O@..O.#",
                "#O#..O...#",
                "#O..O..O.#",
                "#.OO.O.OO#",
                "#....O...#",
                "##########",
                "",
                "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^",
                "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v",
                "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<",
                "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^",
                "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><",
                "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^",
                ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^",
                "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>",
                "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>",
                "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
                ],
            testResult=Just "10092",
            testResult2=Just "9021"
            },
        AOCTest {
            testData=unlines [
                "########",
                "#..O.O.#",
                "##@.O..#",
                "#...O..#",
                "#.#.O..#",
                "#...O..#",
                "#......#",
                "########",
                "",
                "<^^>>>vv<v>>v<<"
                ],
            testResult=Just "2028",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse id,
        codeParse2=parse (concatMap double),
        codeTest=result push1,
        codeTest2=result push2,
        codeResult=result push1,
        codeResult2=result push2
        }
    }

parse modifyMap = p . span (not . null) . lines
  where p (mp,moves) = (parse2da $ modifyMap $ unlines mp,concat moves)

start = fst . head . filter ((== '@') . snd) . assocs

makeBoxes = fromList . map fst . filter ((== 'O') . snd) . assocs

move wall push state '^' = push wall state (0,-1)
move wall push state '>' = push wall state (1,0)
move wall push state 'v' = push wall state (0,1)
move wall push state '<' = push wall state (-1,0)
move wall push state _ = state

gpsCoord (x,y) = x+100*y

result push (mp,moves) =
    sum $ map gpsCoord $ elems $ snd
        $ foldl (move ((== '#') . (mp!)) push) (start mp,makeBoxes mp) moves

push1 :: ((Int,Int) -> Bool) -> ((Int,Int),Set (Int,Int)) -> (Int,Int) -> ((Int,Int),Set (Int,Int))
push1 wall state@((x,y),boxes) (dx,dy) =
    maybe state ((,) (x+dx,y+dy)) $ doPush (x+dx,y+dy)
  where
    doPush xy@(x,y)
      | wall xy = Nothing
      | not (member xy boxes) = Just boxes
      | otherwise = do
          end <- findEnd (x+dx,y+dy)
          return $ insert end $ delete xy boxes
    findEnd xy@(x,y)
      | wall xy = Nothing
      | not (member xy boxes) = Just xy
      | otherwise = findEnd (x+dx,y+dy)

double '#' = "##"
double '.' = ".."
double '@' = "@."
double 'O' = "O."
double c = [c]

push2 :: ((Int,Int) -> Bool) -> ((Int,Int),Set (Int,Int)) -> (Int,Int) -> ((Int,Int),Set (Int,Int))
push2 wall state@((x,y),boxes) (dx,dy) =
    maybe state ((,) (x+dx,y+dy)) $ doPush (x+dx,y+dy)
  where
    doPush xy@(x,y)
      | wall xy = Nothing
      | dx == 1 =
          if member xy boxes
            then fmap pushBoxes $ getPushedXR (x+2,y) [xy]
            else Just boxes
      | dx == -1 =
          if member (x-1,y) boxes
            then fmap pushBoxes $ getPushedXL (x-2,y) [(x-1,y)]
            else Just boxes
      | member xy boxes = fmap pushBoxes $ getPushedY xy [xy]
      | member (x-1,y) boxes = fmap pushBoxes $ getPushedY (x-1,y) [(x-1,y)]
      | otherwise = Just boxes
    getPushedXR xy@(x,y) pushed
      | wall xy = Nothing
      | member xy boxes = getPushedXR (x+2,y) (xy:pushed)
      | otherwise = Just pushed
    getPushedXL xy@(x,y) pushed
      | wall xy = Nothing
      | member (x-1,y) boxes = getPushedXL (x-2,y) ((x-1,y):pushed)
      | otherwise = Just pushed
    getPushedY xy@(x,y) pushed
      | wall (x,y+dy) || wall (x+1,y+dy) = Nothing
      | otherwise = do
          pushed1 <- if member (x-1,y+dy) boxes
                       then getPushedY (x-1,y+dy) ((x-1,y+dy):pushed)
                       else Just pushed
          pushed2 <- if member (x,y+dy) boxes
                       then getPushedY (x,y+dy) ((x,y+dy):pushed1)
                       else Just pushed1
          pushed3 <- if member (x+1,y+dy) boxes
                       then getPushedY (x+1,y+dy) ((x+1,y+dy):pushed2)
                       else Just pushed2
          Just pushed3
    pushBoxes pushed = union (difference boxes (difference removes adds))
                             (difference adds removes)
      where
        removes = fromList pushed
        adds = fromList $ map pushBox pushed
        pushBox (x,y) = (x+dx,y+dy)
