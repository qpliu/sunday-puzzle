import Data.Char(isDigit)

parse :: String -> [(Int,(Int,Int))]
parse s = map (parseDisc . words) $ lines s
  where
    parseDisc ("Disc":disc:"has":positions:"positions;":"at":"time=0,":"it":"is":"at":"position":position:_) = (read positions,(read (filter isDigit disc),read (filter isDigit position)))
    parseDisc w = error (show w)

adjustForFallingTime :: (Int,(Int,Int)) -> (Int,Int)
adjustForFallingTime (positions,(dropTime,positionAtTime0)) = (positions,(positionAtTime0 + dropTime) `mod` positions)

findPushTime :: [(Int,Int)] -> Int
findPushTime discs = search ((maxPositions - maxLagAdjustedPosition) `mod` maxPositions)
  where
    (maxPositions,maxLagAdjustedPosition) = maximum discs
    aligned t (positions,lagAdjustedPosition) = (lagAdjustedPosition+t) `mod` positions == 0
    search t | all (aligned t) discs = t | otherwise = search (t+maxPositions)

test :: ()
test
  | findPushTime (map adjustForFallingTime (parse testData)) /= 5 = error "a"
  | otherwise = ()
  where
    testData = "Disc #1 has 5 positions; at time=0, it is at position 4.\nDisc #2 has 2 positions; at time=0, it is at position 1."

part1 :: IO Int
part1 = fmap (findPushTime . map adjustForFallingTime . parse) $ readFile "input/15.txt"

addPart2 :: [(Int,(Int,Int))] -> [(Int,(Int,Int))]
addPart2 [bottomDisc@(_,(discNumber,_))] = [bottomDisc,(11,(discNumber+1,0))]
addPart2 (disc:discs) = disc:addPart2 discs

part2 :: IO Int
part2 = fmap (findPushTime . map adjustForFallingTime . addPart2 . parse) $ readFile "input/15.txt"
