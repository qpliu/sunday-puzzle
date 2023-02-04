import Data.Char(isDigit)
import Data.Map(Map,alter,empty,keysSet,(!))
import Data.Set(Set,elems,fromList,intersection)

parse :: String -> [Set (Int,(Int,Int))]
parse = parseWire 0 0 []
  where
    parseWire _ _ w [] = [fromList w]
    parseWire x y w (c:cs)
      | c == '\n' = fromList w : parseWire 0 0 [] cs
      | c == ',' = parseWire x y w cs
      | c == 'D' = path (read nstr) 0 1
      | c == 'L' = path (read nstr) (-1) 0
      | c == 'R' = path (read nstr) 1 0
      | c == 'U' = path (read nstr) 0 (-1)
      where
        (nstr,rest) = span isDigit cs
        path n dx dy = parseWire (x+n*dx) (y+n*dy) ([(abs (x+i*dx) + abs (y+i*dy),(x+i*dx,y+i*dy)) | i <- [1..n]] ++ w) rest

find :: [Set (Int,(Int,Int))] -> Int
find (a:b:_) = fst $ minimum (a  `intersection` b)

testData :: [((Int,Int),String)]
testData = [
    ((6,30),"R8,U5,L5,D3\nU7,R6,D4,L4"),
    ((159,610),"R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"),
    ((135,410),"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
    ]

test :: ()
test
  | any (\ ((n,_),str) -> n /= find (parse str)) testData = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (find . parse) $ readFile "input/03.txt"

parse2 :: String -> [Map (Int,Int) Int]
parse2 = parseWire 0 0 0 empty
  where
    parseWire _ _ _ w [] = [w]
    parseWire x y nsteps w (c:cs)
      | c == '\n' = w : parseWire 0 0 0 empty cs
      | c == ',' = parseWire x y nsteps w cs
      | c == 'D' = path (read nstr) 0 1
      | c == 'L' = path (read nstr) (-1) 0
      | c == 'R' = path (read nstr) 1 0
      | c == 'U' = path (read nstr) 0 (-1)
      where
        (nstr,rest) = span isDigit cs
        path n dx dy = parseWire (x+n*dx) (y+n*dy) (nsteps+n) (foldr update w [(nsteps+i,(x+i*dx,y+i*dy)) | i <- [1..n]]) rest
        update (steps,xy) wire = alter (Just . maybe steps id) xy wire

find2 :: [Map (Int,Int) Int] -> Int
find2 (a:b:_) = minimum $ map (\ xy -> a!xy + b!xy)  $ elems $ intersection (keysSet a) (keysSet b)

test2 :: ()
test2
  | any (\ ((_,n),str) -> n /= find2 (parse2 str)) testData = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (find2 . parse2) $ readFile "input/03.txt"
