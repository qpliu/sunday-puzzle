import Debug.Trace(traceShow)
import Data.List(group,intercalate)
import Data.Map(Map,fromList,member,(!))

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

debug a b = traceShow (a,b) b

count2 :: ([(Char,Int)],[Int]) -> Int
count2 (recs,ns) = c recs ns
  where
    c :: [(Char,Int)] -> [Int] -> Int
    c [] [] = 1
    c [] _ = 0
    c ((_,nrec):rest) ns | nrec <= 0 = c rest ns
    c recs []
      | any ((== '#') . fst) recs = 0
      | otherwise = 1
    c (('.',_):rest) ns = c rest ns
    c (('#',nrec):rest) (n:ns)
      | n < nrec = 0
      | otherwise = finishGroup rest ns (n - nrec)
    c (('?',nrec):rest) (n:ns)
      | totaln > nquest + nhash || nhash > totaln = 0
      | otherwise = damCount rest ns n nrec []
      where
        totaln = n + sum ns
        nquest = nrec + sum (map snd $ filter ((== '?') . fst) rest)
        nhash = sum (map snd $ filter ((== '#') . fst) rest)
    finishGroup :: [(Char,Int)] -> [Int] -> Int -> Int
    finishGroup [] ns n
      | n > 0 = 0
      | otherwise = c [] ns
    finishGroup (('.',_):rest) ns n
      | n > 0 = 0
      | otherwise = c rest ns
    finishGroup (('#',nrec):rest) ns n
      | n < nrec = 0
      | otherwise = finishGroup rest ns (n-nrec)
    finishGroup (('?',nrec):rest) ns n
      | n+1 < nrec = c (('?',nrec-n-1):rest) ns
      | n+1 == nrec = c rest ns
      | otherwise = finishGroup rest ns (n-nrec)
    damCount :: [(Char,Int)] -> [Int] -> Int -> Int -> [Int] -> Int
    damCount rest ns n nQs previousGroups
      | remainingQs < 0 = 0
      | otherwise =
          -- push this group of n entirely out of this set of ?
          wtfe (length previousGroups,remainingQs)*c rest (n:ns) +
          -- hang this group of n off the right edge of this set of ?
          sum [wtfe (length previousGroups,remainingQs-m) * finishGroup rest ns (n-m) | m <- [1..n], remainingQs-m >= 0] +
          -- fit this group of n entirely in this set of ?
          (if n+1 > remainingQs then 0
           else if null ns then wtfe (1+length previousGroups,remainingQs-n-1)*c rest ns
           else damCount rest (tail ns) (head ns) nQs (n:previousGroups))
      where
        remainingQs = nQs - sum previousGroups - length previousGroups

result2 :: String -> Int
result2 = sum . map (count2 . parse2 . unfold 5 "?") . parse

test2 :: ()
test2
  | result2 testData /= 525152 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/12.txt"
