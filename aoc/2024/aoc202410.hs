module AOC202410 where

import Data.List(nub)
import Data.Map(Map,fromList,toList,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="10",
    testData=unlines [
    "89010123",
    "78121874",
    "87430965",
    "96549874",
    "45678903",
    "32019012",
    "01329801",
    "10456732"
    ],
    testResult="36",
    testData2="",
    testResult2="81",
    aocParse=parse2d,
    aocResult=result,
    aocParse2=parse2d,
    aocResult2=result2
    }

result = sum . map getScore . toList . makeScores
  where
    getScore (_,('0',score)) = length score
    getScore _ = 0

makeScores mp = scores
  where
    scores = fromList $ map makeScore $ toList mp
    makeScore (xy@(x,y),c)
      | c == '9' = (xy,(c,[xy]))
      | otherwise = (xy,(c,nub $ concat [getScore (x+dx,y+dy) (succ c)
                                | (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)]]))
    getScore xy c
      | Just c == Data.Map.lookup xy mp = snd $ scores!xy
      | otherwise = []

result2 = sum . map getRating . toList . makeRatings
  where
    getRating (_,('0',rating)) = rating
    getRating _ = 0

makeRatings mp = ratings
  where
    ratings = fromList $ map makeRating $ toList mp
    makeRating (xy@(x,y),c)
      | c == '9' = (xy,(c,1))
      | otherwise = (xy,(c,sum [getRating (x+dx,y+dy) (succ c)
                                | (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)]]))
    getRating xy c
      | Just c == Data.Map.lookup xy mp = snd $ ratings!xy
      | otherwise = 0
