start :: (Int,([Int],(Int,Int)))
start = (2,([7,3],(0,1)))

step :: (Int,([Int],(Int,Int))) -> (Int,([Int],(Int,Int)))
step (len,(scores,(index1,index2))) =
    (newlen,(newscores,(newindex1,newindex2)))
  where
    score1 = scores!!(len-1-index1)
    score2 = scores!!(len-1-index2)
    comb = score1+score2
    (newscores,newlen)
      | comb < 10 = (comb:scores,len+1)
      | otherwise = ((comb `mod` 10):(comb `div` 10):scores,len+2)
    newindex1 = (index1 + score1 + 1) `mod` newlen
    newindex2 = (index2 + score2 + 1) `mod` newlen

next10 :: Int -> String
next10 n = get $ dropWhile ((< (n+10)) . fst) $ iterate step start
  where
    get ((len,(scores,_)):_) = concatMap show $ reverse $ take 10 $ drop (len-(10+n)) scores

test :: ()
test
  | next10 9 /= "5158916779" = error "a"
  | next10 5 /= "0124515891" = error "b"
  | next10 18 /= "9251071085" = error "c"
  | next10 2018 /= "5941429882" = error "d"
  | otherwise = ()

-- This is too slow.  The Go code is fast enough.

part1 :: Int -> String
part1 = next10
