import Data.Map(Map,alter,delete,findMax,fromList,insert,lookupGE,member,toList,(!))

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

{-
-- this slow, about 0.6s for my input
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
-}

-- this is much faster: about 60ms for my input
compact :: [((Int,Int),Int)] -> [(Int,Int)]
compact disk = c 0 disk (reverse disk)
  where
    c gap forward@(((fileid,filesize),filegap):next)
             back@(((backid,backsize),backgap):prev)
      | backid == fileid = [(backid,backsize)]
      | gap == 0 = (fileid,filesize) : c filegap next back
      | gap < backsize = (backid,gap) : (fileid,filesize) :
            c filegap next (((backid,backsize-gap),backgap+gap):prev)
      | otherwise = (backid,backsize) : c (gap-backsize) forward prev

checksum :: Int -> Int -> [(Int,Int)] -> Int
checksum n i [] = n
checksum n i ((fileid,size):rest) =
    checksum (n+fileid*sum [i..i+size-1]) (i+size) rest

result = checksum 0 0 . compact

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

{-
-- using doubly linked lists is faster: about 17s for my input
-- this gets the answer wrong: 7441282148965, should be 6272188244509
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
-}

-- looking up the leftmost large enough gap is faster: about 210ms for my input
-- take advantage of the fact that gap sizes can only go up to 9, to
-- quickly find the left-most gap that the block fits in
-- well, gaps can be bigger than 9, but those can only be right of the
-- file being moved
toDoublyLinkedList :: [((Int,Int),Int)] -> Map Int (Int,Int,Int,Int,Int)
toDoublyLinkedList = fromList . toEntries 0
  where
    toEntries _ [] = []
    toEntries blockidx (((fileid,size),gap):rest) =
        (fileid,(blockidx,size,gap,fileid+1,fileid-1)) : toEntries (blockidx+size+gap) rest

toGapMap :: Map Int (Int,Int,Int,Int,Int) -> Map (Int,Int) Int
toGapMap = fromList . filter ((> 0) . fst . fst) . map toGapPointer . toList
  where toGapPointer (fileid,(blockidx,_,gap,_,_)) = ((gap,blockidx),fileid)

compact2 :: Int -> Map (Int,Int) Int -> Map Int (Int,Int,Int,Int,Int)
                                     -> Map Int (Int,Int,Int,Int,Int)
compact2 moveid gapMap disk2
  | moveid <= 0 = disk2
  | otherwise = findGap (movesize+1) Nothing $ lookupGE (movesize,0) gapMap
  where
    (moveblockidx,movesize,movegap,movenext,moveprev) = disk2!moveid
    findGap _ Nothing Nothing =
        compact2 (moveid-1) (delete (movegap,moveblockidx) gapMap) disk2
    findGap _ (Just bestGap) Nothing = domove bestGap
    findGap minGap Nothing (Just firstGap)
      | minGap > 9 = domove firstGap
      | otherwise =
          findGap (minGap+1) (Just firstGap) $ lookupGE (minGap,0) gapMap
    findGap minGap (Just bestGap@((_,bestIdx),_))
                   (Just nextGap@((_,nextIdx),_))
      | minGap > 9 && bestIdx < nextIdx = domove bestGap
      | minGap > 9 = domove nextGap
      | bestIdx < nextIdx =
          findGap (minGap+1) (Just bestGap) $ lookupGE (minGap,0) gapMap
      | otherwise =
          findGap (minGap+1) (Just nextGap) $ lookupGE (minGap,0) gapMap
    domove (gapid,fileid) = compact2 (moveid-1) newGapMap newDisk2
      where
        (fileblockidx,filesize,filegap,filenext,fileprev) = disk2!fileid
        (prevblockidx,prevsize,prevgap,prevnext,prevprev) = disk2!moveprev
        (newGapMap,newDisk2)
          | fileid == moveprev =
              (delete gapid $ delete (movegap,moveblockidx) gapMap,
               insert fileid (fileblockidx,filesize,0,filenext,fileprev) $
               insert moveid (fileblockidx+filesize,movesize,filegap+movegap,
                              movenext,moveprev) disk2)
          | fileblockidx < moveblockidx =
              (delete gapid $ delete (movegap,moveblockidx) $
               delete (prevgap,prevblockidx) $
               insert (filegap-movesize,fileblockidx+filesize) moveid gapMap,
               insert fileid (fileblockidx,filesize,0,moveid,fileprev) $
               alter (fmap (\ (i,s,g,n,p) -> (i,s,g,n,moveid))) filenext $
               insert moveid (fileblockidx+filesize,movesize,filegap-movesize,
                              filenext,fileid) $
               alter (fmap (\ (i,s,g,n,p) -> (i,s,g,n,moveprev))) movenext $
               insert moveprev (prevblockidx,prevsize,prevgap+movesize+movegap,
                                movenext,prevprev) disk2)
          | otherwise = (gapMap,disk2)

checksum2 :: Map Int (Int,Int,Int,Int,Int) -> Int
checksum2 = sum . map c . toList
  where
    c (fileid,(blockidx,size,_,_,_)) = fileid*sum [blockidx..blockidx+size-1]

result2 disk = checksum2 $ compact2 fileid gapMap disk2
  where
    disk2 = toDoublyLinkedList disk
    (fileid,_) = findMax disk2
    gapMap = toGapMap disk2
