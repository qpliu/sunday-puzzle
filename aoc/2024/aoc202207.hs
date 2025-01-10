module AOC202207 where

import Data.Map(Map,elems,fromList,(!))

import AOC

aoc = AOC {
    day="../../2022/input/07",
    aocTests=[
        AOCTest {
            testData=unlines [
                "$ cd /",
                "$ ls",
                "dir a",
                "14848514 b.txt",
                "8504156 c.dat",
                "dir d",
                "$ cd a",
                "$ ls",
                "dir e",
                "29116 f",
                "2557 g",
                "62596 h.lst",
                "$ cd e",
                "$ ls",
                "584 i",
                "$ cd ..",
                "$ cd ..",
                "$ cd d",
                "$ ls",
                "4060174 j",
                "8033020 d.log",
                "5626152 d.ext",
                "7214296 k"
                ],
            testResult=Just "95437",
            testResult2=Just "24933642"
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

data Node = Dir [String] [Node] | File String Int

parse input = nodes
  where
    nodes = fromList $ interp [] $ map words $ lines input
    interp _ [] = []
    interp cwd (["$","cd",".."]:rest) = interp (tail cwd) rest
    interp cwd (["$","cd",dir]:rest) = interp (dir:cwd) rest
    interp cwd (["$","ls"]:rest) = interpLS cwd [] rest
    interp cwd input = error (show (cwd,input))
    interpLS cwd files [] = [(cwd,Dir cwd files)]
    interpLS cwd files rest@(("$":_):_) = (cwd,Dir cwd files) : interp cwd rest
    interpLS cwd files (["dir",dir]:rest) =
        interpLS cwd (nodes!(dir:cwd):files) rest
    interpLS cwd files ([size,file]:rest) =
        interpLS cwd (File file (read size):files) rest

du :: Node -> Int
du (Dir _ entries) = sum $ map du entries
du (File _ size) = size

result = sum . map du . filter ((<= 100000) . du) . elems

result2 nodes = minimum $ filter (>= minSize) $ map du $ elems nodes
  where
    totalSpace = 70000000
    neededSpace = 30000000
    usedSpace = du $ nodes!["/"]
    minSize = neededSpace - (totalSpace - usedSpace)
