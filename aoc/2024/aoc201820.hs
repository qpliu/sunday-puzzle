module AOC201820 where

import Data.Map(Map,alter,member,(!))
import qualified Data.Map
import Data.Set(Set,insert,difference,elems,minView,singleton,union,unions)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2018/input/20",
    aocTests=[
        AOCTest {
            testData="^WNE$",
            testResult=Just "3",
            testResult2=Nothing
            },
        AOCTest {
            testData="^ENWWW(NEEE|SSE(EE|N))$",
            testResult=Just "10",
            testResult2=Nothing
            },
        AOCTest {
            testData="^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$",
            testResult=Just "18",
            testResult2=Nothing
            },
        AOCTest {
            testData="^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$",
            testResult=Just "23",
            testResult2=Nothing
            },
        AOCTest {
            testData="^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$",
            testResult=Just "31",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=undefined,
        codeResult=result,
        codeResult2=result2
        }
    }

type XY = (Int,Int)

follow :: Map XY (Set XY) -> XY -> Set XY -> (XY,String)
       -> (Map XY (Set XY),Set (XY,String))
follow grid branchStart previousBranches (xy@(x,y),(dir:dirs))
  | dir == '$' || dir == ')' =
      (grid,Data.Set.map (flip (,) dirs) previousBranches)
  | dir == 'W' = follow (link xy (x-1,y) grid) branchStart
                        previousBranches ((x-1,y),dirs)
  | dir == 'N' = follow (link xy (x,y-1) grid) branchStart
                        previousBranches ((x,y-1),dirs)
  | dir == 'E' = follow (link xy (x+1,y) grid) branchStart
                        previousBranches ((x+1,y),dirs)
  | dir == 'S' = follow (link xy (x,y+1) grid) branchStart
                        previousBranches ((x,y+1),dirs)
  | dir == '|' = follow grid branchStart
                        (insert xy previousBranches) (branchStart,dirs)
  | dir == '(' =
      finishBranch Data.Set.empty $ follow grid xy Data.Set.empty (xy,dirs)
  where
    link xy1 xy2 = alter (Just . maybe (singleton xy1) (insert xy1)) xy2
                 . alter (Just . maybe (singleton xy2) (insert xy2)) xy1
    finishBranch finished (grid1,branches)
      | null branches = (grid1,finished)
      | otherwise = finishBranch (union finished finished2) (grid2,rest)
      where
        Just (branch,rest) = minView branches
        (grid2,finished2) = follow grid1 branchStart previousBranches branch

parse = fst . follow Data.Map.empty (0,0) Data.Set.empty . (,) (0,0) . tail

furthest :: Int -> Set XY -> Set XY -> Map XY (Set XY) -> Int
furthest nsteps current seen area
  | null current = nsteps - 1
  | otherwise = furthest (nsteps+1) next nextSeen area
  where
    nextSeen = union current seen
    next = difference (unions [area!xy | xy <- elems current]) nextSeen

result = furthest 0 (singleton (0,0)) Data.Set.empty

furtherThan :: Int -> Int -> Set XY -> Set XY -> Map XY (Set XY) -> Int
furtherThan threshold nsteps current seen area
  | nsteps >= threshold = Data.Map.size area - Data.Set.size seen
  | otherwise = furtherThan threshold (nsteps+1) next nextSeen area
  where
    nextSeen = union current seen
    next = difference (unions [area!xy | xy <- elems current]) nextSeen

result2 = furtherThan 1000 0 (singleton (0,0)) Data.Set.empty
