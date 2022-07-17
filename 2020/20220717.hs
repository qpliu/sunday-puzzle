import Data.Set(Set,empty,insert,member,size,toList)

collect :: (Set String,Set String,Set String,Set String,Set String) -> String -> (Set String,Set String,Set String,Set String,Set String)
collect sets@(two,three,four,five,seven) word
  | length word == 2 = (insert word two,three,four,five,seven)
  | length word == 3 = (two,insert word three,four,five,seven)
  | length word == 4 = (two,three,insert word four,five,seven)
  | length word == 5 = (two,three,four,insert word five,seven)
  | length word == 7 = (two,three,four,five,insert word seven)
  | otherwise = sets

make :: String -> String -> [String]
make [a,b] [c,d,e,f,g] = [[e,a,b,c,d,f,g],[g,c,d,e,f,a,b]]
make [a,b,c] [d,e,f,g] = [[e,a,b,c,d,f,g],[a,d,e,f,g,b,c]]
make _ _ = []

unmake :: String -> [(String,String)]
unmake [a,b,c,d,e,f,g] = [([b,c],[d,e,a,f,g]),([b,c,d],[e,a,f,g]),([b,c,d,e],[a,f,g]),([b,c,d,e,a],[f,g])]
unmake _ = []

-- brisket bet risk
