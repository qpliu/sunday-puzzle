module AOC201922 where

import Data.List(sort)

import AOC

aoc = AOC {
    day="../../2019/input/22",
    aocTests=[
        AOCTest {
            testData=unlines [
                "deal with increment 7",
                "deal into new stack",
                "deal into new stack"
                ],
            testResult=Just "5",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "cut 6",
                "deal with increment 7",
                "deal into new stack"
                ],
            testResult=Just "6",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "deal with increment 7",
                "deal with increment 9",
                "cut -2"
                ],
            testResult=Just "7",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "deal into new stack",
                "cut -2",
                "deal with increment 7",
                "cut 8",
                "cut -4",
                "deal with increment 7",
                "cut 3",
                "deal with increment 9",
                "deal with increment 3",
                "cut -1"
                ],
            testResult=Just "2",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 10 5,
        codeTest2=undefined,
        codeResult=result 10007 2019,
        codeResult2=result2 119315717514047 101741582076661 2020
        }
    }

parse = reverse . map (p . words) . lines
  where
    p ["deal","with","increment",i] = (read i,0)
    p ["cut",i]                     = (1,-read i)
    p ["deal","into","new","stack"] = (-1,-1)

type Move = (Integer,Integer)
-- Move = (f,r): if the card's initial position is i, its position
-- after the move is f*i + r.

compose :: Integer -> Move -> Move -> Move
compose deckSize (f1,r1) (f2,r2) =
    -- i
    -- f2*i + r2
    -- f1*(f2*i + r2) + r1 = (f1*f2)*i + (f1*r2 + r1)
    ((f1*f2) `mod` deckSize,(f1*r2 + r1) `mod` deckSize)

move :: Integer -> Move -> Integer -> Integer
move deckSize (f,r) i = (f*i+r) `mod` deckSize

result deckSize initialPosition moves = move deckSize m initialPosition
  where m = foldl (compose deckSize) (1,0) moves

invert :: Integer -> (Integer,Integer) -> (Integer,Integer)
invert deckSize (f,r) = (finv,(-finv*r) `mod` deckSize)
  where
    -- p = f*i + r
    -- finv*p = finv*f*i + finv*r
    -- i = finv*p - finv*r
    finv = minv f deckSize
    minv a n = (fst $ egcd a n) `mod` n
    egcd 0 b = (0,1)
    egcd a b = (y - (b `div` a)*x, x) where (x,y) = egcd (b `mod` a) a

pow :: Integer -> Integer -> (Integer,Integer) -> (Integer,Integer)
pow deckSize n fr
  | n == 1 = fr
  | odd n = compose deckSize fr (compose deckSize fr2 fr2)
  | otherwise = compose deckSize fr2 fr2
  where fr2 = pow deckSize (n `div` 2) fr

result2 deckSize shuffleCount finalPosition moves =
    move deckSize m finalPosition
  where
    m = invert deckSize $ pow deckSize shuffleCount
                        $ foldl (compose deckSize) (1,0) moves
