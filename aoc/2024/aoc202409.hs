import Data.Map(Map,alter,findMax,fromList,insert,member,(!))

import AOC

aoc = AOC {
    day="09",
    testData="2333133121414131402",
    testResult="1928",
    testData2="",
    testResult2="2858",
    aocParse=parse,
    aocResult=result,
    aocParse2=parse,
    aocResult2=result2
    }

parse :: String -> [((Int,Int),Int)]
parse = p 0
  where
    p _ [] = []
    p _ "\n" = []
    p fileid [size] = [((fileid,read [size]),0)]
    p fileid [size,'\n'] = [((fileid,read [size]),0)]
    p fileid (size:gap:rest) =
        ((fileid,read [size]),read [gap]) : p (fileid+1) rest

compact :: Int -> [((Int,Int),Int)] -> [(Int,Int)]
compact _ [] = []
compact _ [((fileid,size),_)] = [(fileid,size)]
compact 0 (((fileid,size),gap):rest) = (fileid,size) : compact gap rest
compact gap disk
  | size <= gap = (fileid,size) : compact (gap-size) (init disk)
  | otherwise = (fileid,gap) : compact 0 (init disk ++ [((fileid,size-gap),0)])
  where ((fileid,size),_) = last disk

checksum :: Int -> Int -> [(Int,Int)] -> Int
checksum n i [] = n
checksum n i ((fileid,size):rest) =
    checksum (n+fileid*sum [i..i+size-1]) (i+size) rest

result = checksum 0 0 . compact 0

{-
-- this is very slow, about 60s for my input
compact2 :: Int -> [((Int,Int),Int)] -> [((Int,Int),Int)]
compact2 moveid disk
  | moveid <= 0 = disk
  | otherwise = compact2 (moveid-1) (movefile disk)
  where
    [((_,movesize),movegap)] = filter ((== moveid) . fst . fst) disk
    movefile (((fileid,size),gap):rest)
      | fileid == moveid = (((fileid,size),gap):rest)
      | movesize <= gap =
          ((fileid,size),0):remove (((moveid,movesize),gap-movesize):rest)
      | otherwise = ((fileid,size),gap) : movefile rest
    remove (((fileid1,size1),gap1):((fileid2,size2),gap2):rest)
      | fileid2 == moveid = (((fileid1,size1),gap1+size2+gap2):rest)
      | otherwise = ((fileid1,size1),gap1):remove (((fileid2,size2),gap2):rest)

checksum2 :: Int -> Int -> [((Int,Int),Int)] -> Int
checksum2 n i [] = n
checksum2 n i (((fileid,size),gap):rest) =
    checksum2 (n+fileid*sum [i..i+size-1]) (i+size+gap) rest

result2 disk = checksum2 0 0 $ compact2 (fst $ fst $ last disk) disk
-}

-- using doubly linked lists is faster: about 17s for my input
toDoublyLinkedList :: [((Int,Int),Int)] -> Map Int (Int,Int,Int,Int)
toDoublyLinkedList = fromList . map toEntry
  where toEntry ((fileid,size),gap) = (fileid,(size,gap,fileid+1,fileid-1))

compact2 :: Int -> Int -> Map Int (Int,Int,Int,Int)
                       -> Map Int (Int,Int,Int,Int)
compact2 firstid moveid disk2
  | moveid <= 0 || moveid == firstid = disk2
  | otherwise = trymove firstid firstid
  where
    (movesize,movegap,movenext,moveprev) = disk2!moveid
    trymove firstid checkid
      | checkid == moveid = compact2 firstid (moveid-1) disk2
      | checkgap == 0 = trymove checkid checknext
      | checkgap < movesize = trymove2 firstid checknext
      | otherwise = compact2 firstid (moveid-1) $ domove checkid checkfile
      where checkfile@(_,checkgap,checknext,_) = disk2!checkid
    trymove2 firstid checkid      
      | checkid == moveid = compact2 firstid (moveid-1) disk2
      | checkgap < movesize = trymove2 firstid checknext
      | otherwise = compact2 firstid (moveid-1) $ domove checkid checkfile
      where checkfile@(_,checkgap,checknext,_) = disk2!checkid
    domove checkid (checksize,checkgap,checknext,checkprev)
      | checknext == moveid =
          insert checkid (checksize,0,checknext,checkprev) $
          insert moveid (movesize,movegap+checkgap,movenext,moveprev) disk2
      | otherwise =
          insert checkid (checksize,0,moveid,checkprev) $
          alter (fmap (\ (s,g,n,p) -> (s,g,n,moveid))) checknext $
          insert moveid (movesize,checkgap-movesize,checknext,checkid) $
          alter (fmap (\ (s,g,n,p) -> (s,g,n,moveprev))) movenext $
          alter (fmap (\ (s,g,n,p) -> (s,g+movegap+movesize,movenext,p)))
                 moveprev disk2

checksum2 :: Int -> Int -> Int -> Map Int (Int,Int,Int,Int) -> Int
checksum2 n i fileid disk2
  | not (member fileid disk2) = n
  | otherwise =
      checksum2 (n+fileid*sum [i..i+size-1]) (i+size+gap) nextid disk2
  where (size,gap,nextid,_) = disk2!fileid

result2 disk = checksum2 0 0 0 $ compact2 0 (fst $ findMax disk2) disk2
  where disk2 = toDoublyLinkedList disk
