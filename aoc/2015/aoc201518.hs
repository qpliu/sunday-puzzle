import Data.Set(Set,fromList,insert,member,size)

parse :: String -> ((Int,Int),Set (Int,Int))
parse s = ((maximum (map length rows),length rows),fromList (concatMap (parseRow) (zip [1..] rows)))
  where
    rows = lines s
    parseRow (row,gridLine) = map (((,) row) . fst) $ filter ((== '#') . snd) $ zip [1..] gridLine

-- This code is kind of slow
step :: ((Int,Int),Set (Int,Int)) -> ((Int,Int),Set (Int,Int))
step ((rows,columns),grid) = ((rows,columns),fromList [(r,c) | r <- [1..rows], c <- [1..columns], on r c])
  where
    on r c
      | (r,c) `member` grid = neighborhood r c `elem` [3,4]
      | otherwise = neighborhood r c == 3
    neighborhood r c = length [() | dr <- [-1,0,1], dc <- [-1,0,1], (r+dr,c+dc) `member` grid]

test 
  | step s0 /= s1 = error "a"
  | step s1 /= s2 = error "b"
  | step s2 /= s3 = error "c"
  | step s3 /= s4 = error "d"
  | otherwise = ()
  where
    s0 = parse ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####.."
    s1 = parse "..##..\n..##.#\n...##.\n......\n#.....\n#.##.."
    s2 = parse "..###.\n......\n..###.\n......\n.#....\n.#...."
    s3 = parse "...#..\n......\n...#..\n..##..\n......\n......"
    s4 = parse "......\n......\n..##..\n..##..\n......\n......"

part1 :: IO Int
part1 = fmap (size . snd . head . drop 100 . iterate step . parse) (readFile "input/18.txt")

addCorners :: ((Int,Int),Set (Int,Int)) -> ((Int,Int),Set (Int,Int))
addCorners ((rows,columns),grid) = ((rows,columns),insert (1,1) $ insert (1,columns) $ insert (rows,1) $ insert (rows,columns) grid)

part2 :: IO Int
part2 = fmap (size . snd . head . drop 100 . iterate (addCorners . step) . addCorners . parse) (readFile "input/18.txt")
