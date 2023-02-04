import Data.List(sort)
import Data.Map(Map,fromList,(!))
import Data.Tuple(swap)

-- Internally, use (y,x) to make sorting carts easier.
parse :: String -> (Map (Int,Int) Char,[((Int,Int),(Char,Int))])
parse s = (fromList tracks,carts)
  where
    (tracks,carts) = p 0 0 [] [] s
    p y x tracks carts "" = (tracks,carts)
    p y x tracks carts ('-':rest) = p y (x+1) (((y,x),'-'):tracks) carts rest
    p y x tracks carts ('|':rest) = p y (x+1) (((y,x),'|'):tracks) carts rest
    p y x tracks carts ('/':rest) = p y (x+1) (((y,x),'/'):tracks) carts rest
    p y x tracks carts ('\\':rest) = p y (x+1) (((y,x),'\\'):tracks) carts rest
    p y x tracks carts ('+':rest) = p y (x+1) (((y,x),'+'):tracks) carts rest
    p y x tracks carts ('v':rest) = p y (x+1) (((y,x),'|'):tracks) (((y,x),('v',0)):carts) rest
    p y x tracks carts ('^':rest) = p y (x+1) (((y,x),'|'):tracks) (((y,x),('^',0)):carts) rest
    p y x tracks carts ('<':rest) = p y (x+1) (((y,x),'-'):tracks) (((y,x),('<',0)):carts) rest
    p y x tracks carts ('>':rest) = p y (x+1) (((y,x),'-'):tracks) (((y,x),('>',0)):carts) rest
    p y x tracks carts ('\n':rest) = p (y+1) 0 tracks carts rest
    p y x tracks carts (_:rest) = p y (x+1) tracks carts rest

tick :: Map (Int,Int) Char -> [((Int,Int),(Char,Int))] -> ([(Int,Int)],[((Int,Int),(Char,Int))])
tick tracks carts = t [] carts []
  where
    t moved [] collisions = (collisions,sort moved)
    t moved (((y,x),(dir,state)):unmoved) collisions
      | (y,x) `elem` collisions = t moved unmoved collisions
      | otherwise = t ((newyx,newdirstate):moved) unmoved newcollisions
      where
        newyx
          | dir == '^' = (y-1,x)
          | dir == '<' = (y,x-1)
          | dir == 'v' = (y+1,x)
          | dir == '>' = (y,x+1)
        newtrack = tracks!newyx
        newdirstate
          | newtrack == '-' = (dir,state)
          | newtrack == '|' = (dir,state)
          | newtrack == '/' && dir == '^' = ('>',state)
          | newtrack == '/' && dir == '>' = ('^',state)
          | newtrack == '/' && dir == 'v' = ('<',state)
          | newtrack == '/' && dir == '<' = ('v',state)
          | newtrack == '\\' && dir == '^' = ('<',state)
          | newtrack == '\\' && dir == '<' = ('^',state)
          | newtrack == '\\' && dir == 'v' = ('>',state)
          | newtrack == '\\' && dir == '>' = ('v',state)
          | newtrack == '+' && state == 0 = (turn dir,1)
          | newtrack == '+' && state == 1 = (dir,2)
          | newtrack == '+' && state == 2 = (turn $ turn $ turn dir,0)
        turn '^' = '<'
        turn '<' = 'v'
        turn 'v' = '>'
        turn '>' = '^'
        newcollisions
          | newyx `elem` (map fst moved ++ map fst unmoved) = (newyx:collisions)
          | otherwise = collisions

-- swap changes Y,X to X,Y for the final result.
run :: String -> (Int,Int)
run s = swap $ last $ fst $ head $ dropWhile (null . fst) $ iterate (tick tracks . snd) ([],carts)
  where (tracks,carts) = parse s

testData :: String
testData = "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   "

test :: ()
test
  | run testData /= (7,3) = error "a"
  | otherwise = ()

part1 :: IO (Int,Int)
part1 = fmap run $ readFile "input/13.txt"

testData2 :: String
testData2 = "/>-<\\  \n|   |  \n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/"

removeCrashed :: ([(Int,Int)],[((Int,Int),(Char,Int))]) -> [((Int,Int),(Char,Int))]
removeCrashed (collisions,carts) = filter (not . (`elem` collisions) . fst) carts

run2 :: String -> (Int,Int)
run2 s = swap $ fst $ head $ head $ dropWhile ((> 1) . length) $ iterate (removeCrashed . tick tracks) carts
  where (tracks,carts) = parse s

test2 :: ()
test2
  | run2 testData2 /= (6,4) = error "a"
  | otherwise = ()

part2 :: IO (Int,Int)
part2 = fmap run2 $ readFile "input/13.txt"
