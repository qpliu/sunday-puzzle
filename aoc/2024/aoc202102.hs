module AOC202102 where

import AOC

aoc = AOC {
    day="../../2021/input/02",
    aocTests=[
        AOCTest {
            testData=unlines [
                "forward 5",
                "down 5",
                "forward 8",
                "up 3",
                "down 8",
                "forward 2"
                ],
            testResult=Just "150",
            testResult2=Just "900"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse :: String -> [(Int,Int)]
parse = map (p . words) . lines
  where
    p ["forward",n] = (read n,0)
    p ["up",n] = (0,-read n)
    p ["down",n] = (0,read n)

result course = (sum $ map fst course) * (sum $ map snd course)

command :: ((Int,Int),Int) -> (Int,Int) -> ((Int,Int),Int)
command ((hPos,vPos),aim) (forward,reaim) =
    ((hPos+forward,vPos+forward*aim),aim+reaim)

result2 = uncurry (*) . fst . foldl command ((0,0),0)
