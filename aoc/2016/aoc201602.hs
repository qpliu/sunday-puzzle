import Data.Map(Map,fromList,member,(!))

makePad :: Map (Int,Int) Char
makePad = fromList [((1,1),'1'),((2,1),'2'),((3,1),'3'),((1,2),'4'),((2,2),'5'),((3,2),'6'),((1,3),'7'),((2,3),'8'),((3,3),'9')]

button :: Map (Int,Int) Char -> ((Int,Int),String) -> String -> ((Int,Int),String)
button pad ((x,y),revcode) "" = ((x,y),pad!(x,y):revcode)
button pad ((x,y),revcode) (insn:insns)
  | (newx,newy) `member` pad = button pad ((newx,newy),revcode) insns
  | otherwise = button pad ((x,y),revcode) insns
  where
    newx | insn == 'R' = x+1 | insn == 'L' = x-1 | otherwise = x
    newy | insn == 'U' = y-1 | insn == 'D' = y+1 | otherwise = y

follow :: String -> String
follow insns = reverse $ snd $ foldl (button makePad) ((2,2),"") (lines insns)

test :: ()
test
  | follow "ULL\nRRDDD\nLURDL\nUUUUD" /= "1985" = error "a"
  | otherwise = ()

part1 :: IO String
part1 = fmap follow $ readFile "input/02.txt"

makePad2 :: Map (Int,Int) Char
makePad2 = fromList [((3,1),'1'),((2,2),'2'),((3,2),'3'),((4,2),'4'),((1,3),'5'),((2,3),'6'),((3,3),'7'),((4,3),'8'),((5,3),'9'),((2,4),'A'),((3,4),'B'),((4,4),'C'),((3,5),'D')]

follow2 :: String -> String
follow2 insns = reverse $ snd $ foldl (button makePad2) ((1,3),"") (lines insns)

test2 :: ()
test2
  | follow2 "ULL\nRRDDD\nLURDL\nUUUUD" /= "5DB3" = error "a"
  | otherwise = ()

part2 :: IO String
part2 = fmap follow2 $ readFile "input/02.txt"
