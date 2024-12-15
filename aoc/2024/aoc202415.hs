module AOC202415 where

import Data.Map(empty,insert,keys,member,toList,(!))

import AOC

aoc = AOC {
    day="15",
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
    testResult="10092",
    testData2="",
    testResult2="9021",
    aocParse=parse id,
    aocTest=result push1,
    aocResult=result push1,
    aocParse2=parse (concatMap double),
    aocTest2=result push2,
    aocResult2=result push2
    }

parse modifyMap = p . span (not . null) . lines
  where p (mp,moves) = (parse2d $ modifyMap $ unlines mp,concat moves)

start = fst . head . filter ((== '@') . snd) . toList

move push state '^' = push state (0,-1)
move push state '>' = push state (1,0)
move push state 'v' = push state (0,1)
move push state '<' = push state (-1,0)
move push state _ = state

push1 state@((x,y),mp) (dx,dy) = maybe state doPush $ findDest (x+dx,y+dy)
  where
    doPush (x2,y2) =
        ((x+dx,y+dy),
         insert (x,y) '.' $ insert (x+dx,y+dy) (mp!(x,y)) $
         insert (x2,y2) (mp!(x2-dx,y2-dy)) mp)
    findDest (x2,y2)
      | ch == 'O' = findDest (x2+dx,y2+dy)
      | ch == '#' = Nothing
      | otherwise = Just (x2,y2)
      where ch = mp!(x2,y2)

gpsCoord ((x,y),ch)
  | ch == 'O' || ch == '[' = x+100*y
  | otherwise = 0

result push (mp,moves) = sum $ map gpsCoord $ toList $ snd
                             $ foldl (move push) ((start mp),mp) moves

double '#' = "##"
double '.' = ".."
double '@' = "@."
double 'O' = "[]"
double c = [c]

push2 state@((x,y),mp) (dx,dy) =
    maybe state ((,) (x+dx,y+dy) . doPush) $ getPushed (x,y) empty
  where
    doPush xys = foldr move1 mp $ keys xys
      where
        move1 (x,y) newmp
          | member (x-dx,y-dy) xys = insert (x+dx,y+dy) (mp!(x,y)) newmp
          | otherwise = insert (x,y) '.' $ insert (x+dx,y+dy) (mp!(x,y)) newmp
    getPushed (x,y) xys
      | ch == '#' = Nothing
      | ch == '.' = Just $ insert (x,y) () xys
      | ch == '[' && dy == 0 = getPushed (x+dx,y+dy) $ insert (x,y) () xys
      | ch == ']' && dy == 0 = getPushed (x+dx,y+dy) $ insert (x,y) () xys
      | ch == '[' = do
          xysLeft <- getPushed (x,y+dy) $ insert (x,y) () xys
          getPushed (x+1,y+dy) xysLeft
      | ch == ']' = do
          xysRight <- getPushed (x,y+dy) $ insert (x,y) () xys
          getPushed (x-1,y+dy) xysRight
      | otherwise = error (show (ch,x,y,dx,dy))
      where ch = mp!(x+dx,y+dy)
