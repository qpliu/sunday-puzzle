module AOC202012 where

import AOC

aoc = AOC {
    day="../../2020/input/12",
    aocTests=[
        AOCTest {
            testData=unlines [
                "F10",
                "N3",
                "F7",
                "R90",
                "F11"
                ],
            testResult=Just "25",
            testResult2=Just "286"
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

parse = map p . lines
  where p (c:num) = (c,read num :: Int)

dist :: (Int,Int) -> Int
dist (x,y) = abs x + abs y

move :: ((Int,Int),(Int,Int)) -> (Char,Int) -> ((Int,Int),(Int,Int))
move ((x,y),(dx,dy)) insn@(action,value)
  | action == 'N' = ((x,y-value),(dx,dy))
  | action == 'S' = ((x,y+value),(dx,dy))
  | action == 'E' = ((x+value,y),(dx,dy))
  | action == 'W' = ((x-value,y),(dx,dy))
  | action == 'F' = ((x+value*dx,y+value*dy),(dx,dy))

  | insn == ('L',90)  || insn == ('R',270) = ((x,y),(dy,-dx))
  | insn == ('L',180) || insn == ('R',180) = ((x,y),(-dx,-dy))
  | insn == ('L',270) || insn == ('R',90)  = ((x,y),(-dy,dx))

result = dist . fst . foldl move ((0,0),(1,0))

move2 :: ((Int,Int),(Int,Int)) -> (Char,Int) -> ((Int,Int),(Int,Int))
move2 ((x,y),(dx,dy)) insn@(action,value)
  | action == 'N' = ((x,y),(dx,dy-value))
  | action == 'S' = ((x,y),(dx,dy+value))
  | action == 'E' = ((x,y),(dx+value,dy))
  | action == 'W' = ((x,y),(dx-value,dy))
  | action == 'F' = ((x+value*dx,y+value*dy),(dx,dy))

  | insn == ('L',90)  || insn == ('R',270) = ((x,y),(dy,-dx))
  | insn == ('L',180) || insn == ('R',180) = ((x,y),(-dx,-dy))
  | insn == ('L',270) || insn == ('R',90)  = ((x,y),(-dy,dx))

result2 = dist . fst . foldl move2 ((0,0),(10,-1))
