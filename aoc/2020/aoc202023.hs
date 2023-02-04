import Data.List(elemIndex)
import Data.Map(Map,findWithDefault,fromList,insert)

move :: Ord a => [a] -> [a]
move (a1:a2:a3:a4:rest) = left ++ (dest:a2:a3:a4:tail right) ++ [a1]
  where
    lesser = filter (< a1) rest
    dest | null lesser = maximum rest | otherwise = maximum lesser
    (left,right) = span (/= dest) rest

reorder :: Ord a => [a] -> [a]
reorder as = right ++ left
  where (left,right) = span (/= (minimum as)) as

test :: ()
test
  | (tail . reorder . head . drop 10 . iterate move) "389125467" /= "92658374" = error "a"
  | (tail . reorder . head . drop 100 . iterate move) "389125467" /= "67384529" = error "b"
  | otherwise = ()

part1 :: Ord a => [a] -> [a]
part1 = tail . reorder . head . drop 100 . iterate move

initial2 :: ([Int],Int) -> (Int -> Int,Int -> Int,Int) -- (cup to position,position to cup,max position)
initial2 (list,maxPos) = (cupToPos,posToCup,maxPos)
  where
    cupToPos cup = maybe cup (+1) $ elemIndex cup list
    posToCup pos | pos <= length list = list !! (pos-1) | otherwise = pos

move2 :: (Int -> Int,Int -> Int,Int) ->  (Int -> Int,Int -> Int,Int)
move2 (cupToPos,posToCup,maxPos) = (nextCupToPos,nextPosToCup,maxPos)
  where
    currentCup = posToCup 1
    targetCup = findTargetCup (currentCup - 1)
    targetPos = cupToPos targetCup
    movedCups = map posToCup [2,3,4]

    nextCupToPos cup
      | pos == 1 = maxPos
      | pos == 2 = targetPos - 3
      | pos == 3 = targetPos - 2
      | pos == 4 = targetPos - 1
      | pos <= targetPos = pos - 4
      | otherwise = pos - 1
      where
        pos = cupToPos cup
    nextPosToCup pos
      | pos <= targetPos - 4 = posToCup (pos+4)
      | pos == targetPos - 3 = posToCup 2
      | pos == targetPos - 2 = posToCup 3
      | pos == targetPos - 1 = posToCup 4
      | pos == maxPos = posToCup 1
      | otherwise = posToCup (pos+1)

    findTargetCup target
      | target <= 0 = findTargetCup maxPos
      | target `elem` movedCups = findTargetCup (target-1)
      | otherwise = target

-- Can't figure out a way how to do this for a whole cycle of moves.
doLoop2 :: (Int -> Int,Int -> Int,Int) -> (Int -> Int,Int -> Int,Int)
doLoop2 (cupToPos,posToCup,maxPos) = (nextCupToPos,nextPosToCup,maxPos)
  where
    nextCupToPos cup = undefined
    nextPosToCup pos = undefined

run2 :: ([Int],Int) -> (Int,Int)
run2 (list,maxPos) = (posToCup (pos1+1),posToCup (pos1+2))
  where
    ((cupToPos,posToCup,_):_) = drop 40 $ iterate doLoop2 (initial2 (list,maxPos))
    pos1 = cupToPos 1

-- After internet hint, have to brute-force it, but without doing tons
-- of copies per move.
initial3 :: [Int] -> Int -> (Int,Int,Map Int Int)
initial3 inits len
  | len > length inits =
      (head (inits++[1]),len,fromList $ zip (len:inits) (inits ++ [length inits+1]))
  | otherwise =
      (head inits,length inits,fromList $ zip (last inits:init inits) inits)

move3 :: (Int,Int,Map Int Int) -> (Int,Int,Map Int Int)
move3 (current,len,links) = (nextCurrent,len,nextLinks)
  where
    after n = findWithDefault (n+1) n links
    move1 = after current
    move2 = after move1
    move3 = after move2
    target = findTarget (current-1)

    nextCurrent = after move3
    nextLinks = insert current nextCurrent
              $ insert move3 (after target)
              $ insert target move1 links

    findTarget t
      | t <= 0 = findTarget len
      | t == move1 || t == move2 || t == move3 = findTarget (t-1)
      | otherwise = t

run3 :: [Int] -> Int -> Int -> [Int]
run3 initial len moves = takeWhile (/= 1) $ tail $ iterate after 1
  where
    after n = findWithDefault (n+1) n links
    ((_,_,links):_) = drop moves $ iterate move3 $ initial3 initial len

test2 :: ()
test2
  | take 2 (run3 [3,8,9,1,2,5,4,6,7] 1000000 10000000) /= [934001,159792] = error "a"
  | otherwise = ()

-- Gets stack overflow.  I could learn how to use strictness annotations
-- or just write it in another language.
