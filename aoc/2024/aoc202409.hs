module AOC202409 where

import Data.Set(Set,delete,fromList,insert,lookupGE)
import qualified Data.Set

import AOC

aoc = AOC {
    day="09",
    testData="2333133121414131402",
    testResult="1928",
    testData2="",
    testResult2="2858",
    aocParse=parse,
    aocResult=result,
    aocParse2=parse2,
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

result :: [((Int,Int),Int)] -> Int
result = checksum 0 0 . compact

parse2 :: String -> ([(Int,Int,Int)],Set (Int,Int))
parse2 = p 0 0 [] []
  where
    p _ _ files gaps "" = (files,fromList gaps)
    p _ _ files gaps "\n" = (files,fromList gaps)
    p _ _ files gaps "\r\n" = (files,fromList gaps)
    p fileid blockidx files gaps (f:"") =
        ((fileid,blockidx,read [f]):files,fromList gaps)
    p fileid blockidx files gaps (f:"\n") =
        ((fileid,blockidx,read [f]):files,fromList gaps)
    p fileid blockidx files gaps (f:"\r\n") =
        ((fileid,blockidx,read [f]):files,fromList gaps)
    p fileid blockidx files gaps (f:g:rest) =
        p (fileid+1) (blockidx+size+gap)
          ((fileid,blockidx,size):files) ((gap,blockidx+size):gaps)
          rest
      where
        size = read [f]
        gap = read [g]

checksum2 :: (Int,Set (Int,Int)) -> (Int,Int,Int) -> (Int,Set (Int,Int))
checksum2 (total,gaps) (fileid,blockidx,size) =
    maybe (total + fileid*(blockidx*size + ((size*(size-1)) `div` 2)),gaps)
          chksumMoved $ findGap (size+1) Nothing
                      $ lookupGE (size,0) gaps
  where
    findGap minsize best Nothing
      | minsize > 9 = best
      | otherwise =
          findGap (minsize+1) best $ lookupGE (minsize,0) gaps
    findGap minsize Nothing first@(Just (firstSize,firstIdx))
      | minsize > 9 && firstIdx < blockidx = first
      | minsize > 9 = Nothing
      | firstIdx > blockidx =
          findGap (minsize+1) Nothing $ lookupGE (minsize,0) gaps
      | otherwise =
          findGap (minsize+1) first $ lookupGE (minsize,0) gaps
    findGap minsize best@(Just (bestSize,bestIdx))
                    next@(Just (nextSize,nextIdx))
      | minsize > 9 = if nextIdx < bestIdx then next else best
      | nextIdx < bestIdx =
          findGap (minsize+1) next $ lookupGE (minsize,0) gaps
      | otherwise =
          findGap (minsize+1) best $ lookupGE (minsize,0) gaps
    chksumMoved (gapsize,gapidx) =
        (total + fileid*(gapidx*size + ((size*(size-1)) `div` 2)),
         delete (gapsize,gapidx) $
           if gapsize <= size
             then gaps
             else insert (gapsize-size,gapidx+size) gaps)

result2 :: ([(Int,Int,Int)],Set (Int,Int)) -> Int
result2 (files,gaps) = fst $ foldl checksum2 (0,gaps) files
