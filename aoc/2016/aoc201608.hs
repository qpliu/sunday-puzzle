import Data.Char(isDigit)
import Data.Set(Set,empty,fromList,member,size,union)
import qualified Data.Set

interpret :: (Int,Int) -> Set (Int,Int) -> String -> Set (Int,Int)
interpret dims@(w,h) set insn = interp (words insn)
  where
    interp ("rect":wxh:_) = fromList [(x,y) | x <- [0 .. min w (read (takeWhile isDigit wxh)) - 1], y <- [0 .. min h (read (takeWhile isDigit $ dropWhile (not . isDigit) $ dropWhile isDigit wxh)) - 1]] `union` set
    interp ("rotate":"column":x:"by":n:_) = Data.Set.map (rotcol (read $ filter isDigit x) (read n)) set
    interp ("rotate":"row":y:"by":n:_) = Data.Set.map (rotrow (read $ filter isDigit y) (read n)) set
    interp _ = error ("unrecognized:"++insn)
    rotcol col by (x,y) | x == col = (x,(y+by)`mod`h) | otherwise = (x,y)
    rotrow row by (x,y) | y == row = ((x+by)`mod`w,y) | otherwise = (x,y)

parse :: String -> Set (Int,Int)
parse s = fromList $ map fst $ filter ((== '#') . snd) $ concat $ zipWith parseRow [0..] $ lines s
  where
    parseRow row str = zip (map (flip (,) row) [0..]) str

test :: ()
test
  | interpret dim empty "rect 3x2" /= step1 = error "a"
  | interpret dim step1 "rotate column x=1 by 1" /= step2 = error "b"
  | interpret dim step2 "rotate row y=0 by 4" /= step3 = error "c"
  | interpret dim step3 "rotate column x=1 by 1" /= step4 = error "d"
  | otherwise = ()
  where
    dim = (7,3)
    step1 = parse "###....\n###....\n......."
    step2 = parse "#.#....\n###....\n.#....."
    step3 = parse "....#.#\n###....\n.#....."
    step4 = parse ".#..#.#\n#.#....\n.#....."

part1 :: IO Int
part1 = fmap (size . foldl (interpret (50,6)) empty . lines) $ readFile "input/08.txt"

display :: (Int,Int) -> Set (Int,Int) -> String
display (w,h) set = unlines [[if (x,y) `member` set then '#' else '.' | x <- [0..w-1]]| y <- [0..h-1]]

part2 :: IO ()
part2 = readFile "input/08.txt" >>= putStrLn . display (50,6) . foldl (interpret (50,6)) empty . lines
