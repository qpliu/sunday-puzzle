module AOC201520 where

import AOC

aoc = AOC {
    day="../../2015/input/20",
    aocTests=[
        AOCTest {
            testData="100",
            testResult=Just "6",
            testResult2=Nothing
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

parse = head . parseInts

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

search :: Int -> Int -> [(Int,(Int,[Int]))] -> [Int] -> Int
search target ubound previous (p:ps)
  | null candidates = ubound -- not sure this is correct, but it works for my input
  | otherwise = search target (minimum (ubound:candidates)) next ps
  where
    byPrev :: [([(Int,(Int,[Int]))],[(Int,(Int,[Int]))])]
    byPrev = [span ((< target) . fst) [makeHouse prev i | i <- [0..]]
              | prev <- previous]

    next :: [(Int,(Int,[Int]))]
    next = [house | (houses,_) <- byPrev,
                    house@(_,(number,_)) <- houses,
                    number < ubound]
    candidates :: [Int]
    candidates = [number | (_,houses) <- byPrev,
                           (_,(number,_)) <- take 1 houses,
                           number < ubound]

    makeHouse :: (Int,(Int,[Int])) -> Int -> (Int,(Int,[Int]))
    makeHouse (_,(previousNumber,previousElves)) i =
        (sum $ map (10*) elves,(previousNumber*p^i,elves))
      where
        elves = [elf*p^j | elf <- previousElves, j <- [0..i]]

result n = search n n [(10,(1,[1]))] primes

search2 :: Int -> Int -> [(Int,(Int,[Int]))] -> [Int] -> Int
search2 target ubound previous (p:ps)
  | null candidates = ubound -- not sure this is correct, but it works for my input
  | otherwise = search2 target (minimum (ubound:candidates)) next ps
  where
    byPrev :: [([(Int,(Int,[Int]))],[(Int,(Int,[Int]))])]
    byPrev = [span ((< target) . fst) [makeHouse prev i | i <- [0..]]
              | prev <- previous]

    next :: [(Int,(Int,[Int]))]
    next = [house | (houses,_) <- byPrev,
                    house@(_,(number,_)) <- houses,
                    number < ubound]
    candidates :: [Int]
    candidates = [number | (_,houses) <- byPrev,
                           (_,(number,_)) <- take 1 houses,
                           number < ubound]

    makeHouse :: (Int,(Int,[Int])) -> Int -> (Int,(Int,[Int]))
    makeHouse (_,(previousNumber,previousElves)) i =
        (sum $ map presents elves,(previousNumber*p^i,elves))
      where
        number = previousNumber*p^i
        elves = [elf*p^j | elf <- previousElves, j <- [0..i]]
        presents elf
          | 50*elf >= number = 11*elf
          | otherwise = 0

result2 n = search2 n n [(11,(1,[1]))] primes
