module AOC202025 where

import AOC

aoc = AOC {
    day="../../2020/input/25",
    aocTests=[
        AOCTest {
            testData=unlines [
                "5764801",
                "17807724"
                ],
            testResult=Just "14897079",
            testResult2=Nothing
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parseInts,
        pcodeParse2=const (const ()),
        pcodeTest=result,
        pcodeTest2=const (const ()),
        pcodeResult=result,
        pcodeResult2=const (const ())
        }
    }

r :: Int
r = 20201227

mpow :: Int -> Int -> Int
mpow s 0 = 1
mpow s n
  | even n = (mpow s (n `div` 2))^2 `mod` r
  | otherwise = (s*mpow s (n-1)) `mod` r

search :: Int -> (Int,Int) -> [(Int,Int)] -> [Int] -> Int
search ncpu step@(stepN,stepF) starts keys@[pk1,pk2]
  | null results = search ncpu step ends keys
  | otherwise = head results
  where
    searches = parallelMap ncpu (search1 1000) starts
    results = concatMap (either (const []) (:[])) searches
    ends = map (either id (error "can't happen")) searches

    search1 i (n,f)
      | f == pk1 = Right $ mpow pk2 n
      | f == pk2 = Right $ mpow pk1 n
      | i <= 0 = Left (n+stepN,(f*stepF) `mod` r)
      | otherwise = search1 (i-1) (n+stepN,(f * stepF) `mod` r)

result ncpu = search ncpu (ncpu,(7^ncpu `mod` r))
                          [(i,7^i `mod` r) | i <- [0..ncpu-1]]
