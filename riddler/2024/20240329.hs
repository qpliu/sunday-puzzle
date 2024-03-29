import Data.Map(Map,(!),empty,filterWithKey,fromList,insert,member)

makeTable :: Int -> Int -> Map (Int,Int) Int
makeTable asize bsize = tabulate empty [((0,0),0)]
  where
    tabulate table [] = table
    tabulate table (((a,b),n):queue)
      | member (a,b) table && table!(a,b) <= n = tabulate table queue
      | otherwise = tabulate (insert (a,b) n table) (queue ++
          (if a > 0 then [((0,b),n+1)] else []) ++
          (if b > 0 then [((a,0),n+1)] else []) ++
          (if a < asize then [((asize,b),n+1)] else []) ++
          (if b < bsize then [((a,bsize),n+1)] else []) ++
          (if a+b <= asize then [((a+b,0),n+1)] else [((asize,a+b-asize),n+1)]) ++
          (if a+b <= bsize then [((0,a+b),n+1)] else [((a+b-bsize,bsize),n+1)]))
          
f :: Int -> Int -> Map Int Int
f asize bsize = fromList [(n,makef n) | n <- [0..max asize bsize]]
  where
    table = makeTable asize bsize
    makef n
      | ntable == empty = -1
      | otherwise = minimum ntable
      where ntable = filterWithKey (\ (a,b) _ -> a == n || b == n) table
