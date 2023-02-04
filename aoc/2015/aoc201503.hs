import Data.Set(Set,fromList,insert,size)

deliver :: ((Integer,Integer),Set (Integer,Integer)) -> Char -> ((Integer,Integer),Set (Integer,Integer))
deliver ((x,y),set) insn
  | insn == '^' = ((x,y-1),insert (x,y-1) set)
  | insn == 'v' = ((x,y+1),insert (x,y+1) set)
  | insn == '<' = ((x-1,y),insert (x-1,y) set)
  | insn == '>' = ((x+1,y),insert (x+1,y) set)
  | otherwise = ((x,y),set)

deliveries :: String -> Int
deliveries insns = size $ snd $ foldl deliver ((0,0),fromList [(0,0)]) insns

main :: IO ()
main = getContents >>= print . deliveries

test :: ()
test
  | deliveries ">" /= 2 = error "a"
  | deliveries "^>v<" /= 4 = error "b"
  | deliveries "^v^v^v^v^v" /= 2 = error "c"
  | otherwise = ()

part1 :: IO Int
part1 = fmap deliveries (readFile "input/03.txt")

part2a :: ((Integer,Integer),(Integer,Integer),Set (Integer,Integer)) -> Char -> ((Integer,Integer),(Integer,Integer),Set (Integer,Integer))
part2a ((x,y),b,set) insn
  | insn == '^' = ((x,y-1),b,insert (x,y-1) set)
  | insn == 'v' = ((x,y+1),b,insert (x,y+1) set)
  | insn == '<' = ((x-1,y),b,insert (x-1,y) set)
  | insn == '>' = ((x+1,y),b,insert (x+1,y) set)
  | otherwise = ((x,y),b,set)

part2b :: ((Integer,Integer),(Integer,Integer),Set (Integer,Integer)) -> Char -> ((Integer,Integer),(Integer,Integer),Set (Integer,Integer))
part2b (a,(x,y),set) insn
  | insn == '^' = (a,(x,y-1),insert (x,y-1) set)
  | insn == 'v' = (a,(x,y+1),insert (x,y+1) set)
  | insn == '<' = (a,(x-1,y),insert (x-1,y) set)
  | insn == '>' = (a,(x+1,y),insert (x+1,y) set)
  | otherwise = (a,(x,y),set)

part2c :: String -> Int
part2c insns = size $ runa ((0,0),(0,0),fromList [(0,0)]) insns
  where
    runa (_,_,set) "" = set
    runa state (insn:insns) = runb (part2a state insn) insns
    runb (_,_,set) "" = set
    runb state (insn:insns) = runa (part2b state insn) insns

part2 :: IO Int
part2 = fmap part2c (readFile "input/03.txt")
