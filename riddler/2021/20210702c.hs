import Data.List(permutations)
import Data.Time(getCurrentTime)

maxScore :: (Num a, Ord a) => [a] -> (a,[(a,a)])
maxScore [] = (0,[])
maxScore [_] = (0,[])
maxScore [a,b] = (a*b,[(a,b)])
maxScore [a,b,c] | a < min b c = (b*c,[(b,c)])
                 | otherwise = let d = max b c in (a*d,[(a,d)])
maxScore (a:dots) =
    maximum (maxScore dots:[scoreSplit (splitAt i dots) | i <- [0..length dots-1]])
  where
    scoreSplit (loop1,b:loop2) = (a*b+s1+s2,(a,b):p1++p2)
      where
        (s1,p1) = maxScore loop1
        (s2,p2) = maxScore loop2

search :: Int -> Int -> ((Int,[(Int,Int)]),[Int]) -> [[Int]] -> IO ()
search blocksize n best arrangements
  | null arrangements = return ()
  | otherwise = do
      getCurrentTime >>= print
      let blockbest = minimum [(maxScore dots,dots) | dots <- take blocksize arrangements]
      print (n,product [3..10],blockbest,best)
      search blocksize (n+blocksize) (min blockbest best) (drop blocksize arrangements)

main :: IO ()
main = do
    print (maxScore [1..11],250)
    print (maxScore [1,4,8,7,11,2,5,9,3,6,10],237)
    search 1000 0 ((250,[]),[]) (map (1:) (filter twobeforethree (permutations [2..11])))
    -- search 1000 1600000 ((224,[]),[1,4,2,8,5,10,7,11,6,9,3]) (map (1:) (drop 1600000 (filter twobeforethree (permutations [2..11])))) -- skip the first 60 hours of searching
    -- non-exhaustive list of arrangements with max score of 224:
    --  [1,8,5,10,7,11,6,9,4,2,3]
    --  [1,9,6,10,7,11,5,8,4,2,3]
    --  [1,4,2,8,5,10,7,11,6,9,3]
    --  [1,4,2,8,5,11,7,10,6,9,3]
    --  [1,4,2,9,6,10,7,11,5,8,3]
    --  [1,6,11,7,10,5,9,4,8,2,3]
    --  [1,7,10,6,11,5,9,4,8,2,3]
    --  [1,8,4,9,5,10,7,11,6,2,3]
    --  [1,8,4,9,5,10,6,11,7,2,3]
    --  [1,4,2,3,8,5,10,7,11,6,9]
    --  [1,4,2,3,9,6,10,7,11,5,8]
  where
    twobeforethree [] = True
    twobeforethree (2:_) = True
    twobeforethree (3:_) = False
    twobeforethree (_:dots) = twobeforethree dots
