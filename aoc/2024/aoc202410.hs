module AOC202410 where

import Data.Map(elems,fromList,singleton,size,toList,unionsWith,(!))
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
    aocTest=result,
    aocResult=result,
    aocParse2=parse2d,
    aocTest2=result2,
    aocResult2=result2
    }

score metric grid = sum $ map (metric . snd) $ filter fst $ elems trails
  where
    trails = fromList $ map getTrails $ toList grid
    getTrails (xy@(x,y),c)
      | c == '9' = (xy,(False,singleton xy 1))
      | otherwise =
          (xy,(c == '0',
               unionsWith (+)
                   [snd $ trails!(x+dx,y+dy)
                    | (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)],
                      Just (succ c) == Data.Map.lookup (x+dx,y+dy) grid]))

result = score size

result2 = score sum
