import Data.Map(Map,alter,empty,filterWithKey)
import qualified Data.Map

sizes :: Int -> [Int]
sizes width = [width*width - (m*(m+1)) `div` 2 | m <- [0..width-1]]

counts :: Int -> Int -> Map Int [Int]
counts minWidth maxWidth = filterWithKey complete $ foldl collect empty $ concat [map ((,) width) (sizes width) | width <- [minWidth..maxWidth]]
  where
    collect m (width,count) = alter (Just . maybe [width] (width:)) count m
    complete size _ = size <= (maxWidth*(maxWidth+1)) `div` 2 && size >= minWidth*minWidth
