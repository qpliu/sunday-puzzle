module AOC202511 where

import Data.Map(findWithDefault,fromList,toList,(!))

import AOC

aoc = AOC {
    day="11",
    aocTests=[
        AOCTest {
            testData=unlines [
                "aaa: you hhh",
                "you: bbb ccc",
                "bbb: ddd eee",
                "ccc: ddd eee fff",
                "ddd: ggg",
                "eee: out",
                "fff: out",
                "ggg: out",
                "hhh: ccc fff iii",
                "iii: out"
                ],
            testResult=Just "5",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "svr: aaa bbb",
                "aaa: fft",
                "fft: ccc",
                "bbb: tty",
                "tty: ccc",
                "ccc: ddd eee",
                "ddd: hub",
                "hub: fff",
                "eee: dac",
                "dac: fff",
                "fff: ggg hhh",
                "ggg: out",
                "hhh: out"
                ],
            testResult=Nothing,
            testResult2=Just "2"
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

parse = map (p . words) . lines
  where p (k:outs) = (init k,outs)

result graph = memo!"you"
  where
    memo = fromList $ map ways graph
    ways (k,outs)
      | outs == ["out"] = (k,1)
      | otherwise = (k,sum [memo!out | out <- outs])

result2 graph = get ("svr",True,True)
  where
    memo = fromList $ concatMap ways graph
    get k = findWithDefault 0 k memo
    ways (k,outs)
      | outs == ["out"] = [((k,False,False),1)]
      | k == "dac" =
          [((k,True,False),sum [get (out,True,False) | out <- outs] +
                           sum [get (out,False,False) | out <- outs]),
           ((k,True,True),sum [get (out,True,True) | out <- outs] +
                          sum [get (out,False,True) | out <- outs])]
      | k == "fft" =
          [((k,False,True),sum [get (out,False,True) | out <- outs] +
                           sum [get (out,False,False) | out <- outs]),
           ((k,True,True),sum [get (out,True,True) | out <- outs] +
                          sum [get (out,True,False) | out <- outs])]
      | otherwise =
          [((k,dac,fft),sum [get (out,dac,fft) | out <- outs])
           | dac <- [True,False], fft <- [True,False]]
