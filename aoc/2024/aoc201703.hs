module AOC201703 where

import AOC

aoc = AOC {
    day="../../2017/input/03",
    aocTests=[
        AOCTest {
            testData="1",
            testResult=Just "0",
            testResult2=Just "2"
            },
        AOCTest {
            testData="12",
            testResult=Just "3",
            testResult2=Just "23"
            },
        AOCTest {
            testData="23",
            testResult=Just "2",
            testResult2=Just "25"
            },
        AOCTest {
            testData="1024",
            testResult=Just "31",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=head . parseInts,
        codeParse2=head . parseInts,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

dist :: Int -> Int
dist n = f $ floor $ sqrt $ fromIntegral n
  where
    f k
      | even k = f $ k-1
      | k^2 > n = f $ k-2
      | (k+2)^2 <= n = f $ k+2
      | k^2 == n = k-1
      | otherwise =
          (k+1) `div` 2 + abs ((n - k^2) `mod` (k+1) - (k+1) `div` 2)

result = dist

spiral :: [Int]
spiral = 1:1:2:4:5:10:11:23:25:26:54:f [1,2,4,5,10,11,23,25] [54,26] 3 3
  where
    f [i1,i2] outer@(o1:_) innerW i =
        n1:n2:n3:n4:f newInner [n4,n3] (innerW+2) 3
      where
        n1 = o1+i1+i2+newi1
        n2 = n1+i2+newi1
        n3 = n2+newi1
        n4 = n3+n2+newi1+newi2
        newInner@(newi1:newi2:_) = reverse (n2:n1:outer)
    f (i1:inner@(i2:i3:_)) outer@(o1:_) innerW i | i `mod` innerW /= 0 =
        n1:f inner (n1:outer) innerW (i+1)
      where n1 = o1+i1+i2+i3
    f (i1:inner@(i2:i3:_)) outer@(o1:_) innerW i =
        n1:n2:n3:f inner (n3:n2:n1:outer) innerW 2
      where
        n1 = o1+i1+i2
        n2 = n1+i2
        n3 = n2+n1+i2+i3

result2 n = head $ dropWhile (<= n) $ spiral
