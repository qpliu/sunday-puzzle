import Data.Map(Map,empty,findWithDefault,fromList,insert)

-- If ..... => # and ##### => . then there could be infinite number of pots
-- with plants after an odd number of generations.

parse :: String -> (Map String Char,(Int,Char,String,Char))
parse s = (fromList (toRules rules),(0,'.',state,'.'))
  where
    ("initial":"state:":state:rules) = words s
    toRules (src:"=>":[dest]:rest) = (src,dest) : toRules rest
    toRules _ = []

trim :: (Int,Char,String,Char) -> (Int,Char,String,Char)
trim (i,left,pots,right) = (i+length ltrim,left,reverse rpots,right)
  where
    (ltrim,pots2) = span (== left) pots
    rpots = dropWhile (== right) (reverse pots2)

next :: Map String Char -> (Int,Char,String,Char) -> (Int,Char,String,Char)
next rules (i,l,pots,r) = (i-2,newpot [l,l,l,l,l],newpots,newpot [r,r,r,r,r])
  where
    newpots = makeNew (l:l:l:l:pots ++ [r,r,r,r])
    makeNew (p1:rest@(p2:p3:p4:p5:_)) = newpot [p1,p2,p3,p4,p5] : makeNew rest
    makeNew _ = []
    newpot llcrr = findWithDefault '.' llcrr rules

plantNumbers :: (Int,Char,String,Char) -> [Int]
plantNumbers (i,_,pots,_) = map fst $ filter ((== '#') . snd) $ zip [i..] pots

testData :: String
testData = "initial state: #..#.#..##......###...###\n\n...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #"

test :: ()
test
  | sum (plantNumbers $ head $ drop 20 $ iterate (trim . next rules) state) /= 325 = error "a"
  | otherwise = ()
  where (rules,state) = parse testData

part1 :: IO Int
part1 = do
  (rules,state) <- fmap parse $ readFile "input/12.txt"
  return $ sum $ plantNumbers $ head $ drop 20 $ iterate (trim . next rules) state

-- For part 2, look for cycle, or constant growth factor.

-- For my input data, after 153 steps, 53 plants remain in 178 consecutive
-- pots, moving right by one pot with each step.  At 153 steps, the sum of the
-- pot numbers is 8575.

part2 :: Integer -> Integer
part2 n = 8575 + 53*(n - 153)
