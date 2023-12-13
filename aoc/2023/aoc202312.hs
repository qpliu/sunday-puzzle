import Control.Monad.State(State,evalState,get,put)
import Data.List(group,intercalate)
import Data.Map(Map,empty,fromList,insert,member,(!))

toSpace :: Char -> Char -> Char
toSpace target ch | ch == target = ' ' | otherwise = ch

count :: (String,[Int]) -> Int
count (recs,ns) = c recs ns
  where
    c recs []
      | any (== '#') recs = 0
      | otherwise = 1
    c [] _ = 0
    c ('.':recs) ns = c recs ns
    c ('#':recs) (n:ns) = finishGroup recs ns (n-1)
    c ('?':recs) (n:ns) = finishGroup recs ns (n-1) + c recs (n:ns)
    finishGroup [] ns n
      | n <= 0 = c [] ns
      | otherwise = 0
    finishGroup ('.':recs) ns n
      | n <= 0 = c recs ns
      | otherwise = 0
    finishGroup ('#':recs) ns n
      | n <= 0 = 0
      | otherwise = finishGroup recs ns (n-1)
    finishGroup ('?':recs) ns n
      | n <= 0 = c recs ns
      | otherwise = finishGroup recs ns (n-1)

parse :: String -> [(String,[Int])]
parse = map parseLine . lines
  where parseLine line = (recs,map read ns)
          where (recs:ns) = words $ map (toSpace ',') line

result :: String -> Int
result = sum . map count . parse

testData :: String
testData = unlines [
    "???.### 1,1,3",
    ".??..??...?##. 1,1,3",
    "?#?#?#?#?#?#?#? 1,3,1,6",
    "????.#...#... 4,1,1",
    "????.######..#####. 1,6,5",
    "?###???????? 3,2,1"
    ]

test :: ()
test
  | result testData /= 21 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/12.txt"

unfold :: Int -> String -> (String,[Int]) -> (String,[Int])
unfold count joiner (recs,ns) =
    (intercalate joiner $ replicate count recs, concat $ replicate count ns)

parse2 :: (String,[Int]) -> ([(Char,Int)],[Int])
parse2 (recs,ns) = (map reparse $ group recs,ns)
  where reparse rec = (head rec,length rec)

-- The maximum number of consecutive ? in my input is 64, so the maximum
-- number of groups that can fit entirely in a set of ? is 32
wtfe :: (Int,Int) -> Int
wtfe key
  | not (member key wtfeTable) = error (show key)
  | otherwise = wtfeTable!key

wtfeTable :: Map (Int,Int) Int
wtfeTable = table
  where
    table = fromList [((ngroups,ndots),waysToFitEntirely ngroups ndots) | ngroups <- [0..32], ndots <- [0..64 - 2*ngroups]]
    waysToFitEntirely :: Int -> Int -> Int
    waysToFitEntirely ngroups ndots
      | ndots <= 0 = 1
      | ngroups <= 0 = 1
      | ngroups == 1 = ndots + 1  -- all left of the group, all right of the group, or some combination
      | otherwise = sum [table!(ngroups-1,nLeftOfFirst) | nLeftOfFirst <- [0..ndots]]

type Memo = Map ([(Char,Int)],[Int],Int,Int) Int

count2 :: ([(Char,Int)],[Int]) -> Int
count2 (recs,ns) = evalState (c recs ns) empty
  where
    c :: [(Char,Int)] -> [Int] -> State Memo Int
    c [] [] = return 1
    c [] _ = return 0
    c ((_,nrec):rest) ns | nrec <= 0 = c rest ns
    c recs []
      | any ((== '#') . fst) recs = return 0
      | otherwise = return 1
    c (('.',_):rest) ns = c rest ns
    c (('#',nrec):rest) (n:ns)
      | n < nrec = return 0
      | otherwise = finishGroup rest ns (n - nrec)
    c (('?',nrec):rest) (n:ns)
      | totaln > nquest + nhash || nhash > totaln = return 0
      | otherwise = do
          memo <- get
          if (rest,ns,n,nrec) `member` memo then return (memo!(rest,ns,n,nrec))
          else do
            val <- damCount rest ns n nrec []
            memo <- get
            put (insert (rest,ns,n,nrec) val memo)
            return val
      where
        totaln = n + sum ns
        nquest = nrec + sum (map snd $ filter ((== '?') . fst) rest)
        nhash = sum (map snd $ filter ((== '#') . fst) rest)
    finishGroup :: [(Char,Int)] -> [Int] -> Int -> State Memo Int
    finishGroup [] ns n
      | n > 0 = return 0
      | otherwise = c [] ns
    finishGroup (('.',_):rest) ns n
      | n > 0 = return 0
      | otherwise = c rest ns
    finishGroup (('#',nrec):rest) ns n
      | n < nrec = return 0
      | otherwise = finishGroup rest ns (n-nrec)
    finishGroup (('?',nrec):rest) ns n
      | n+1 < nrec = c (('?',nrec-n-1):rest) ns
      | n+1 == nrec = c rest ns
      | otherwise = finishGroup rest ns (n-nrec)
    damCount :: [(Char,Int)] -> [Int] -> Int -> Int -> [Int] -> State Memo Int
    damCount rest ns n nQs previousGroups
      | remainingQs < 0 = return 0
      | otherwise = do
          -- push this group of n entirely out of this set of ?
          v1 <- fmap (wtfe (length previousGroups,remainingQs)*) $ c rest (n:ns)
          -- hang this group of n off the right edge of this set of ?
          v2 <- sequence [fmap (wtfe (length previousGroups,remainingQs-m) *) $ finishGroup rest ns (n-m) | m <- [1..n], remainingQs-m >= 0]
          -- fit this group of n entirely in this set of ?
          v3 <-
              (if n+1 > remainingQs then return 0
               else if null ns then fmap (wtfe (1+length previousGroups,remainingQs-n-1)*) $ c rest ns
               else damCount rest (tail ns) (head ns) nQs (n:previousGroups))
          return (v1+sum v2+v3)
      where
        remainingQs = nQs - sum previousGroups - length previousGroups

result2 :: String -> Int
result2 = sum . map (count2 . parse2 . unfold 5 "?") . parse

{-
-- this approach doesn't work, and only agrees when it's fast anyhow
-- ??? 1
-- when unfolded once becomes
-- ??????? 1,1
-- and
-- #.#.... wouldn't be counted by this approach

c2 :: (String,[Int]) -> Int
c2 item = n1^2 + n2
  where
    n1 = count2 $ parse2 $ item
    n2 = count2 $ parse2 $ unfold 2 "#" item

c3 :: (String,[Int]) -> Int
c3 item = n1^3 + 2*n2*n1 + n3
  where
    n1 = count2 $ parse2 $ item
    n2 = count2 $ parse2 $ unfold 2 "#" item
    n3 = count2 $ parse2 $ unfold 3 "#" item

c4 :: (String,[Int]) -> Int
c4 item = n1^4 + 3*n2*n1^2 + n2^2 + 2*n3*n1 + n4
  where
    n1 = count2 $ parse2 $ item
    n2 = count2 $ parse2 $ unfold 2 "#" item
    n3 = count2 $ parse2 $ unfold 3 "#" item
    n4 = count2 $ parse2 $ unfold 4 "#" item

c5 :: (String,[Int]) -> Int
c5 item = n1^5 + 4*n2*n1^3 + 3*n2^2*n1 + 3*n3*n1^2 + 2*n3*n2 + 2*n4*n1 + n5
  where
    n1 = count2 $ parse2 $ item
    n2 = count2 $ parse2 $ unfold 2 "#" item
    n3 = count2 $ parse2 $ unfold 3 "#" item
    n4 = count2 $ parse2 $ unfold 4 "#" item
    n5 = count2 $ parse2 $ unfold 5 "#" item
-}

test2 :: ()
test2
  | result2 testData /= 525152 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/12.txt"
