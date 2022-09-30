import Data.List(sort)
import Data.Map(Map,alter,empty,toList)

--  1
-- 2 3
--  4
-- 5 6
--  7

segments :: Int -> [Int]
segments 0 = [1,2,3,5,6,7]
segments 1 = [3,6]
segments 2 = [1,3,4,5,7]
segments 3 = [1,3,4,6,7]
segments 4 = [2,3,4,6]
segments 5 = [1,2,4,6,7]
segments 6 = [1,2,4,5,6,7]
segments 7 = [1,3,6]
segments 8 = [1,2,3,4,5,6,7]
segments 9 = [1,2,3,4,6,7]

faded :: [([Int],[(Int,Int)])]
faded = filter ((> 1) . length . snd) $ toList $ foldl collect empty [(sort (segments x ++ segments y),(x,y)) | x <- [0..9], y <- [x..9]]
  where
    collect m (k,v) = alter (Just . maybe [v] (v:)) k m

main :: IO ()
main = print faded
