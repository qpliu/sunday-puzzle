module AOC202510 where

import Data.Bits(shiftL,xor,(.|.))
import Data.List(nub)
import Data.Map(Map,adjust,fromList,toList,(!))
import qualified Data.Map
import Data.Set(empty,insert,member)

import AOC

aoc = AOC {
    day="10",
    aocTests=[
        AOCTest {
            testData=unlines [
                "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}",
                "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}",
                "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
            ],
            testResult=Just "7",
            testResult2=Just "33"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse2,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

parse :: String -> [(Int,[Int],[Int])]
parse = map (p . words) . lines
  where
    p (lights:rest) =
        (plights 0 1 lights,map pswitch (init rest),parseInts (last rest))
    plights bits bit ('[':rest) = plights bits bit rest
    plights bits bit (']':rest) = bits
    plights bits bit ('#':rest) = plights (bits .|. bit) (shiftL bit 1) rest
    plights bits bit ('.':rest) = plights bits (shiftL bit 1) rest
    pswitch = sum . map (shiftL 1) . parseInts

result :: Int -> [(Int,[Int],[Int])] -> Int
result ncpu = parallelMapReduce ncpu countPresses sum

countPresses (0,_,_) = 0
countPresses (goal,switches,_) = count 1 switches [] empty
  where
    count npresses [] queue seen = count (npresses+1) queue [] seen
    count npresses (r:rest) queue seen
      | r == goal = npresses
      | member r seen = count npresses rest queue seen
      | otherwise =
          count npresses rest (map (xor r) switches ++ queue) (insert r seen)

parse2 :: String -> [([[Int]],[Int])]
parse2 = map (p . drop 1 . words) . lines
  where p spec = (map parseInts (init spec),parseInts (last spec))

result2 ncpu = parallelMapReduce ncpu countPresses2 sum

countPresses2 :: ([[Int]],[Int]) -> Int
countPresses2 (buttons,joltages)
  | null bsMatrix = bsPresses
  | otherwise = bsPresses + bfPresses
  where
    initialMaxPresses = [minimum [j | (i,j) <- zip [0..] joltages,
                                      i `elem` button]
                         | button <- buttons]
    initialMatrix =
        [[if i `elem` button then 1 else 0 | button <- buttons]
         | (i,joltage) <- zip [0..] joltages]
    initialRHS = joltages

    (udMaxPresses,udMatrix,udRHS) =
        upperDiagonalize initialMaxPresses initialMatrix initialRHS 0

    (bsPresses,bsMaxPresses,bsMatrix,bsRHS) =
        backsubstitute 0 (reverse udMaxPresses)
                         (reverse (map reverse udMatrix))
                         (reverse udRHS)
                         [] []

    bfPresses = bruteForce bsMaxPresses bsMatrix bsRHS

upperDiagonalize :: [Int] -> [[Int]] -> [Int] -> Int -> ([Int],[[Int]],[Int])
upperDiagonalize maxPresses matrix rhs row
  | row >= length matrix = (maxPresses,matrix,rhs)
  | row >= length pivotRow && pivotRHS == 0 = upperDiagonalize maxPresses mat4 rhs4 row
  | row >= length pivotRow = error "bad input"
  | pivot /= 0 = upperDiagonalize maxPresses mat1Div rhs1Div (row+1)
  | not (null pivotRowIndexes2) = upperDiagonalize maxPresses mat2 rhs2 row
  | not (null pivotColIndexes3) = upperDiagonalize maxPresses3 mat3 rhs row
  | head (drop row rhs) == 0 = upperDiagonalize maxPresses mat4 rhs4 row
  | otherwise = error "bad input"
  where
    pivotRow = head $ drop row matrix
    pivot = head $ drop row pivotRow
    pivotRHS = head $ drop row rhs

    mat1 = take (row+1) matrix ++ map udRow (drop (row+1) matrix)
    rhs1 = take (row+1) rhs ++ map udRhs (drop (row+1) (zip matrix rhs))
    udRow lowerRow = [pivot*l - a*p | (l,p) <- zip lowerRow pivotRow]
      where a = head $ drop row lowerRow
    udRhs (lowerRow,lowerRHS) = pivot*lowerRHS - a*pivotRHS
      where a = head $ drop row lowerRow

    (mat1Div,rhs1Div) = divideGCD mat1 rhs1
    divideGCD [] [] = ([],[])
    divideGCD (row:mat) (r:rhs)
      | f == 0 = (row:matDiv,r:rhsDiv)
      | otherwise = (map (`div` f) row:matDiv,r `div` f:rhsDiv)
      where
        (matDiv,rhsDiv) = divideGCD mat rhs
        f = foldr gcd r row

    pivotRowIndexes2 = [i | (i,lowerRow) <- drop (row+1) (zip [0..] matrix),
                            head (drop row lowerRow) /= 0]
    pivotRowIndex2 = head pivotRowIndexes2
    mat2 = take row matrix ++ drop pivotRowIndex2 matrix
                           ++ drop row (take pivotRowIndex2 matrix)
    rhs2 = take row rhs ++ drop pivotRowIndex2 rhs
                        ++ drop row (take pivotRowIndex2 rhs)

    pivotColIndexes3 = [i | (i,coef) <- drop (row+1) (zip [0..] pivotRow),
                            coef /= 0]
    pivotColIndex3 = head pivotColIndexes3
    mat3 = map shiftCols matrix
    maxPresses3 = shiftCols maxPresses
    shiftCols r = take row r ++ drop pivotColIndex3 r
                             ++ drop row (take pivotColIndex3 r)

    mat4 = take row matrix ++ drop (row+1) matrix
    rhs4 = take row rhs ++ drop (row+1) rhs

backsubstitute :: Int -> [Int] -> [[Int]] -> [Int] -> [[Int]] -> [Int] -> (Int,[Int],[[Int]],[Int])
backsubstitute presses maxPresses [] [] topMatrix topRHS =
    (presses,maxPresses,reverse topMatrix,reverse topRHS)
backsubstitute presses maxPresses (row:matrix) (r:rhs) topMatrix topRHS
  | length coefs == 1 = backsubstitute presses1 maxPresses1 mat1 rhs1 [] []
  | otherwise =
      backsubstitute presses maxPresses matrix rhs (row:topMatrix) (r:topRHS)
  where
    coefs = [(i,c) | (i,c) <- zip [0..] row, c /= 0]
    [(col,coef)] = coefs
    sol
      | r `mod` coef /= 0 = error "bad input"
      | r `div` coef < 0 = error "bad input"
      | r `div` coef > head (drop col maxPresses) = error "bad input"
      | otherwise = r `div` coef

    presses1 = presses+sol
    maxPresses1 = rmCol maxPresses
    mat1 = map rmCol (reverse topMatrix) ++ map rmCol matrix
    rhs1 = map subst (reverse (zip topMatrix topRHS)) ++ map subst (zip matrix rhs)

    rmCol rs = take col rs ++ drop (col+1) rs
    subst (rs,rhsVal) = rhsVal - sol*head (drop col rs)

bruteForce :: [Int] -> [[Int]] -> [Int] -> Int
bruteForce maxPresses mat rhs = foldr solve (sum maxPresses) freeVars
  where
    nFreeVars = length maxPresses - length mat
    freeVars = gen (take nFreeVars maxPresses) (take nFreeVars maxPresses)
      where
        gen current maxes
          | all (== 0) current = [current]
          | otherwise = current : gen (next current maxes) maxes
          where
            next [] [] = []
            next (c:cs) (m:ms)
              | c > 0 = c-1 : cs
              | otherwise = m : next cs ms
    solve :: [Int] -> Int -> Int
    solve sols best = subst (sum sols) sols mat rhs
      where
        subst current sols [] [] = min current best
        subst current sols (row:mat) (r:rhs)
          | current >= best = best
          | any (/= 0) (drop (1 + length sols) row) = error "?"
          | sol < 0 = best
          | sol > head (drop (length sols) maxPresses) = best
          | otherwise = subst (current+sol) (sols ++ [sol]) mat rhs
          where
            rhsSubst = r - sum (zipWith (*) row sols)
            coef = head $ drop (length sols) row
            sol
              | rhsSubst `mod` coef /= 0 = best
              | otherwise = rhsSubst `div` coef
