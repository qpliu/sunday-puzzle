module AOC202210 where

import Data.Map(fromList,(!))

import AOC

aoc = AOC {
    day="../../2022/input/10",
    aocTests=[
        AOCTest {
            testData=unlines [
                "addx 15",
                "addx -11",
                "addx 6",
                "addx -3",
                "addx 5",
                "addx -1",
                "addx -8",
                "addx 13",
                "addx 4",
                "noop",
                "addx -1",
                "addx 5",
                "addx -1",
                "addx 5",
                "addx -1",
                "addx 5",
                "addx -1",
                "addx 5",
                "addx -1",
                "addx -35",
                "addx 1",
                "addx 24",
                "addx -19",
                "addx 1",
                "addx 16",
                "addx -11",
                "noop",
                "noop",
                "addx 21",
                "addx -15",
                "noop",
                "noop",
                "addx -3",
                "addx 9",
                "addx 1",
                "addx -3",
                "addx 8",
                "addx 1",
                "addx 5",
                "noop",
                "noop",
                "noop",
                "noop",
                "noop",
                "addx -36",
                "noop",
                "addx 1",
                "addx 7",
                "noop",
                "noop",
                "noop",
                "addx 2",
                "addx 6",
                "noop",
                "noop",
                "noop",
                "noop",
                "noop",
                "addx 1",
                "noop",
                "noop",
                "addx 7",
                "addx 1",
                "noop",
                "addx -13",
                "addx 13",
                "addx 7",
                "noop",
                "addx 1",
                "addx -33",
                "noop",
                "noop",
                "noop",
                "addx 2",
                "noop",
                "noop",
                "noop",
                "addx 8",
                "noop",
                "addx -1",
                "addx 2",
                "addx 1",
                "noop",
                "addx 17",
                "addx -9",
                "addx 1",
                "addx 1",
                "addx -3",
                "addx 11",
                "noop",
                "noop",
                "addx 1",
                "noop",
                "addx 1",
                "noop",
                "noop",
                "addx -13",
                "addx -19",
                "addx 1",
                "addx 3",
                "addx 26",
                "addx -30",
                "addx 12",
                "addx -1",
                "addx 3",
                "addx 1",
                "noop",
                "noop",
                "noop",
                "addx -9",
                "addx 18",
                "addx 1",
                "addx 2",
                "noop",
                "noop",
                "addx 9",
                "noop",
                "noop",
                "noop",
                "addx -1",
                "addx 2",
                "addx -37",
                "addx 1",
                "addx 3",
                "noop",
                "addx 15",
                "addx -21",
                "addx 22",
                "addx -6",
                "addx 1",
                "noop",
                "addx 2",
                "addx 1",
                "noop",
                "addx -10",
                "noop",
                "noop",
                "addx 20",
                "addx 1",
                "addx 2",
                "addx 2",
                "addx -6",
                "addx -11",
                "noop",
                "noop",
                "noop"
                ],
            testResult=Just "13140",
            testResult2=Just (show $ unlines [
                "",
                "##..##..##..##..##..##..##..##..##..##..",
                "###...###...###...###...###...###...###.",
                "####....####....####....####....####....",
                "#####.....#####.....#####.....#####.....",
                "######......######......######......####",
                "#######.......#######.......#######....."
                ])
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=ocr . drop 1 . result2
        }
    }

parse = map parseInts . lines

execute :: Int -> Int -> [Int] -> [[Int]] -> [Int]
execute cycle x (targetCycle:_) []
  | cycle == targetCycle = [x*cycle]
  | otherwise = []
execute cycle x targetCycles@(targetCycle:nextTargets) ([]:insns)
  | cycle == targetCycle = (x*cycle) : execute (cycle+1) x nextTargets insns
  | otherwise = execute (cycle+1) x targetCycles insns
execute cycle x targetCycles@(targetCycle:nextTargets) ([n]:insns)
  | cycle == targetCycle || cycle+1 == targetCycle =
      (x*targetCycle) : execute (cycle+2) (x+n) nextTargets insns
  | otherwise = execute (cycle+2) (x+n) targetCycles insns

result = sum . execute 1 1 [20,60..]

execute2 :: Int -> Int -> [[Int]] -> String
execute2 cycle x [] = "\n"
execute2 cycle x (insn:insns) = output
  where
    output
      | cycle `mod` 40 == 0 = '\n':pixel
      | otherwise = pixel
    pixel
      | cycle `mod` 40 `elem` [x-1,x,x+1] = '#':exec
      | otherwise = '.':exec
    exec
      | null insn = execute2 (cycle+1) x insns
      | length insn == 1 = execute2 (cycle+1) x ((0:insn):insns)
      | otherwise = execute2 (cycle+1) (x+sum insn) insns

ocr :: String -> String
ocr = recognize . lines
  where
    recognize pixels
      | null $ head pixels = []
      | otherwise = chars!(map (take 4) pixels):recognize (map (drop 5) pixels)
    chars = fromList
        [([".##.",
           "#..#",
           "#..#",
           "####",
           "#..#",
           "#..#"
          ],'A'),
         (["###.",
           "#..#",
           "#..#",
           "###.",
           "#..#",
           "#..#",
           "###."
          ],'B'),
         ([".##.",
           "#..#",
           "#...",
           "#...",
           "#..#",
           ".##."
          ],'C'),
         (["###.",
           "#..#",
           "#..#",
           "#..#",
           "#..#",
           "###."
          ],'D'),
         (["####",
           "#...",
           "####",
           "#...",
           "#...",
           "####"
          ],'E'),
         (["####",
           "#...",
           "###.",
           "#...",
           "#...",
           "#..."
          ],'F'),
         ([".##.",
           "#..#",
           "#...",
           "#.##",
           "#..#",
           ".###"
          ],'G'),
         (["#..#",
           "#..#",
           "####",
           "#..#",
           "#..#",
           "#..#"
          ],'H'),
         (["###.",
           ".#..",
           ".#..",
           ".#..",
           ".#..",
           "###."
          ],'I'),
         ([".###",
           "..#.",
           "..#.",
           "..#.",
           "#.#.",
           ".#.."
          ],'J'),
         (["#..#",
           "#.#.",
           "##..",
           "#.#.",
           "#..#",
           "#..#"
          ],'K'),
         (["#...",
           "#...",
           "#...",
           "#...",
           "#...",
           "####"
          ],'L'),
         (["#..#",
           "####",
           "#..#",
           "#..#",
           "#..#",
           "#..#"
          ],'M'),
         (["#..#",
           "##.#",
           "##.#",
           "#.##",
           "#.##",
           "#..#"
          ],'N'),
         ([".##.",
           "#..#",
           "#..#",
           "#..#",
           "#..#",
           ".##."
          ],'O'),
         (["###.",
           "#..#",
           "#..#",
           "###.",
           "#...",
           "#..."
          ],'P'),
         ([".##.",
           "#..#",
           "#..#",
           "#..#",
           "#.##",
           ".###"
          ],'Q'),
         (["###.",
           "#..#",
           "#..#",
           "###.",
           "#..#",
           "#..#"
          ],'R'),
         ([".##.",
           "#..#",
           ".#..",
           "..#.",
           "#..#",
           ".##."
          ],'S'),
         (["###.",
           ".#..",
           ".#..",
           ".#..",
           ".#..",
           ".#.."
          ],'T'),
         (["#..#",
           "#..#",
           "#..#",
           "#..#",
           "#..#",
           ".##."
          ],'U'),
         (["#..#",
           "#..#",
           "#..#",
           ".##.",
           ".##.",
           ".##."
          ],'V'),
         (["#..#",
           "#..#",
           "#..#",
           "#..#",
           ".##.",
           ".##."
          ],'W'),
         (["#..#",
           "#..#",
           ".##.",
           ".##.",
           "#..#",
           "#..#"
          ],'X'),
         (["#.#.",
           "#.#.",
           "#.#.",
           ".#..",
           ".#..",
           ".#.."
          ],'Y'),
         (["####",
           "...#",
           "..#.",
           ".#..",
           "#...",
           "####"
          ],'Z')
        ]

result2 = execute2 0 1
