module AOC201513 where

import Data.List(permutations)
import Data.Map(Map,alter,empty,(!))
import Data.Set(Set,fromList,elems,insert)

import AOC

aoc = AOC {
    day="../../2015/input/13",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Alice would gain 54 happiness units by sitting next to Bob.",
                "Alice would lose 79 happiness units by sitting next to Carol.",
                "Alice would lose 2 happiness units by sitting next to David.",
                "Bob would gain 83 happiness units by sitting next to Alice.",
                "Bob would lose 7 happiness units by sitting next to Carol.",
                "Bob would lose 63 happiness units by sitting next to David.",
                "Carol would lose 62 happiness units by sitting next to Alice.",
                "Carol would gain 60 happiness units by sitting next to Bob.",
                "Carol would gain 55 happiness units by sitting next to David.",
                "David would gain 46 happiness units by sitting next to Alice.",
                "David would lose 7 happiness units by sitting next to Bob.",
                "David would gain 41 happiness units by sitting next to Carol."
                ],
            testResult=Just "330",
            testResult2=Nothing
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse2,
        pcodeTest=result,
        pcodeTest2=result,
        pcodeResult=result,
        pcodeResult2=result
        }
    }

parse = fmap fromList . foldr collect (empty,[]) . map words . lines
  where
    collect [a,_,sign,count,_,_,_,_,_,_,b] (table,attendees) =
        (alter (Just . maybe amount (+amount))
               (min a (init b),max a (init b)) table,
         a:init b:attendees)
      where
        [n] = parseInts count
        amount | sign == "gain" = n | otherwise = -n

parse2 = add . parse
  where
    add (table,attendees) =
        (foldr addApathy table $ elems attendees,insert "" attendees)
    addApathy attendee = alter (const (Just 0)) ("",attendee)

rate :: Map (String,String) Int -> String -> [String] -> Int
rate table first rest =
    sum $ zipWith r (rest++[first]) (first:rest)
  where
    r a b = table!(min a b,max a b)

result ncpu (table,attendees) =
    parallelMapReduce ncpu (rate table first) maximum $ permutations rest
  where
    (first:rest) = elems attendees
