module AOC201523 where

import Data.Array(Array,array,bounds,inRange,(!))

import AOC

aoc = AOC {
    day="../../2015/input/23",
    aocTests=[
        AOCTest {
            testData=unlines [
                "inc a",
                "jio a, +2",
                "tpl a",
                "inc a"
                ],
            testResult=Just "2",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result fst (0,0),
        codeTest2=undefined,
        codeResult=result snd (0,0),
        codeResult2=result snd (1,0)
        }
    }

parseInsn :: [String] -> (Int,(Int,Int)) -> (Int,(Int,Int))
parseInsn ["hlf","a"] (ip,(a,b)) = (ip+1,(div a 2,b))
parseInsn ["hlf","b"] (ip,(a,b)) = (ip+1,(a,div b 2))
parseInsn ["tpl","a"] (ip,(a,b)) = (ip+1,(3*a,b))
parseInsn ["tpl","b"] (ip,(a,b)) = (ip+1,(a,3*b))
parseInsn ["inc","a"] (ip,(a,b)) = (ip+1,(a+1,b))
parseInsn ["inc","b"] (ip,(a,b)) = (ip+1,(a,b+1))
parseInsn ["jmp",off] (ip,(a,b)) = (ip+offset,(a,b))
  where [offset] = parseInts off
parseInsn ["jie","a,",off] (ip,(a,b))
  | even a = (ip+offset,(a,b))
  | otherwise = (ip+1,(a,b))
  where [offset] = parseInts off
parseInsn ["jie","b,",off] (ip,(a,b))
  | even b = (ip+offset,(a,b))
  | otherwise = (ip+1,(a,b))
  where [offset] = parseInts off
parseInsn ["jio","a,",off] (ip,(a,b))
  | a == 1 = (ip+offset,(a,b))
  | otherwise = (ip+1,(a,b))
  where [offset] = parseInts off
parseInsn ["jio","b,",off] (ip,(a,b))
  | b == 1 = (ip+offset,(a,b))
  | otherwise = (ip+1,(a,b))
  where [offset] = parseInts off

parse input = array (0,length insns-1) $ zip [0..] insns
  where
    insns = map (parseInsn . words) $ lines input

execute :: (Int,Int) -> Array Int ((Int,(Int,Int)) -> (Int,(Int,Int)))
        -> (Int,Int)
execute initialRegs insns = exec (0,initialRegs)
  where
    b = bounds insns
    exec state@(ip,regs)
      | inRange b ip = exec $ (insns!ip) state
      | otherwise = regs

result output input = output . execute input
