module AOC202223 where

import Data.Map(toList)
import Data.Set(Set,elems,empty,fromList,insert,member,size)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2022/input/23",
    aocTests=[
        AOCTest {
            testData=unlines [
                "....#..",
                "..###.#",
                "#...#.#",
                ".#...##",
                "#.###..",
                "##.#.##",
                ".#..#.."
                ],
            testResult=Just "110",
            testResult2=Just "20"
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

parse = fromList . map fst . filter ((== '#') . snd) . toList . parse2d

type XY = (Int,Int)
type Move = Set XY -> XY -> Maybe XY

moveList :: [Move]
moveList = [moveN,moveS,moveW,moveE]
  where
    moveN elves (x,y)
      | member (x-1,y-1) elves || member (x,y-1) elves
                               || member (x+1,y-1) elves = Nothing
      | otherwise = Just (x,y-1)
    moveS elves (x,y)
      | member (x-1,y+1) elves || member (x,y+1) elves
                               || member (x+1,y+1) elves = Nothing
      | otherwise = Just (x,y+1)
    moveW elves (x,y)
      | member (x-1,y-1) elves || member (x-1,y) elves
                               || member (x-1,y+1) elves = Nothing
      | otherwise = Just (x-1,y)
    moveE elves (x,y)
      | member (x+1,y-1) elves || member (x+1,y) elves
                               || member (x+1,y+1) elves = Nothing
      | otherwise = Just (x+1,y)

proposeMove :: [Move] -> Set XY -> XY -> XY
proposeMove moves elves elf@(x,y)
  | member (x+1,y) elves || member (x+1,y+1) elves || member (x,y+1) elves
                         || member (x-1,y+1) elves || member (x-1,y) elves
                         || member (x-1,y-1) elves || member (x,y-1) elves
                         || member (x+1,y-1) elves =
        p moves
  | otherwise = elf
  where
    p [] = elf
    p (move:moves) = maybe (p moves) id $ move elves elf

doRound :: ([Move],Set XY) -> ([Move],Set XY)
doRound (moves,elves) =
    (tail moves,doMoves $ foldr propose (empty,empty,empty) $ elems elves)
  where
    moves4 = take 4 moves
    propose elf (proposedMoves,proposed,collisions)
      | member movedElf proposed =
          (insert (elf,elf) proposedMoves,
           proposed,
           insert movedElf collisions)
      | otherwise =
          (insert (elf,movedElf) proposedMoves,
           insert movedElf proposed,
           collisions)
      where movedElf = proposeMove moves4 elves elf
    doMoves (proposedMoves,_,collisions) =
        Data.Set.map doMove proposedMoves
      where
        doMove (elf,movedElf)
          | member movedElf collisions = elf
          | otherwise = movedElf

bounds :: (a,Set XY) -> Int
bounds (_,elves) =
    (maximum xs-minimum xs+1)*(maximum ys-minimum ys+1) - size elves
  where
    xs = Data.Set.map fst elves
    ys = Data.Set.map snd elves

result = bounds . head . drop 10 . iterate doRound . (,) (cycle moveList)

result2 = find . zip [0..] . iterate doRound . (,) (cycle moveList)
  where
    find ((_,(_,elves1)):rest@((n,(_,elves2)):_))
      | elves1 == elves2 = n
      | otherwise = find rest
