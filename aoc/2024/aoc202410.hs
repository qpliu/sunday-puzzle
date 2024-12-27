module AOC202410 where

import Data.Map(elems,fromList,singleton,size,toList,unionsWith,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="10",
    aocTests=[
        AOCTest {
            testData=unlines [
                "...0...",
                "...1...",
                "...2...",
                "6543456",
                "7.....7",
                "8.....8",
                "9.....9"
                ],
            testResult=Just "2",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "..90..9",
                "...1.98",
                "...2..7",
                "6543456",
                "765.987",
                "876....",
                "987...."
                ],
            testResult=Just "4",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "10..9..",
                "2...8..",
                "3...7..",
                "4567654",
                "...8..3",
                "...9..2",
                ".....01"
                ],
            testResult=Just "3",
            testResult2=Nothing
            },
        AOCTest {
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
            testResult=Just "36",
            testResult2=Just "81"
            },
        AOCTest {
            testData=unlines [
                ".....0.",
                "..4321.",
                "..5..2.",
                "..6543.",
                "..7..4.",
                "..8765.",
                "..9...."
                ],
            testResult=Nothing,
            testResult2=Just "3"
            },
        AOCTest {
            testData=unlines [
                "..90..9",
                "...1.98",
                "...2..7",
                "6543456",
                "765.987",
                "876....",
                "987...."
                ],
            testResult=Nothing,
            testResult2=Just "13"
            },
        AOCTest {
            testData=unlines [
                "012345",
                "123456",
                "234567",
                "345678",
                "4.6789",
                "56789."
                ],
            testResult=Nothing,
            testResult2=Just "227"
            }
        ],
    aocCode=Code {
        codeParse=parse2d,
        codeParse2=parse2d,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
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
