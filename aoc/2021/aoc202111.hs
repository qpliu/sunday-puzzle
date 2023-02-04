import Data.Char(ord)
import Data.Map(Map,adjust,member,toList,(!))
import qualified Data.Map
import Data.Set(Set,difference,elems,empty,fromList,size,union)

parse :: String -> Map (Int,Int) Int
parse = Data.Map.fromList . p 0 0
  where
    p x y ('\n':rest) = p 0 (y+1) rest
    p x y (c:rest) = ((x,y),ord c - ord '0') : p (x+1) y rest
    p _ _ [] = []

step :: (Int,Map (Int,Int) Int) -> (Int,Map (Int,Int) Int)
step (nflashes,ocs0) = flash empty initialFlashers ocs1
  where
    ocs1 = Data.Map.map (+1) ocs0
    initialFlashers = fromList $ map fst $ filter ((> 9) . snd) $ toList ocs1
    flash flashed flashers ocs
      | size flashers == 0 = (nflashes + size flashed,Data.Map.map reset ocs)
      | otherwise = flash newflashed newflashers newocs
        where
          (newflashersList,newocs) = foldr flash1 ([],ocs) (elems flashers)
          newflashed = union flashed flashers
          newflashers = difference (fromList newflashersList) newflashed
    flash1 (x,y) (newflashers,ocs) = foldr getFlashed (newflashers,ocs) [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0, member (x+dx,y+dy) ocs]
    getFlashed xy (newflashers,ocs)
      | ocs!xy >= 9 = (xy:newflashers,adjust (+1) xy ocs)
      | otherwise = (newflashers,adjust (+1) xy ocs)
    reset n | n > 9 = 0 | otherwise = n

testData :: [(Int,String)]
testData = [
    (0,"5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526\n"),
    (1,"6594254334\n3856965822\n6375667284\n7252447257\n7468496589\n5278635756\n3287952832\n7993992245\n5957959665\n6394862637\n"),
    (2,"8807476555\n5089087054\n8597889608\n8485769600\n8700908800\n6600088989\n6800005943\n0000007456\n9000000876\n8700006848\n"),
    (3,"0050900866\n8500800575\n9900000039\n9700000041\n9935080063\n7712300000\n7911250009\n2211130000\n0421125000\n0021119000\n"),
    (4,"2263031977\n0923031697\n0032221150\n0041111163\n0076191174\n0053411122\n0042361120\n5532241122\n1532247211\n1132230211\n"),
    (5,"4484144000\n2044144000\n2253333493\n1152333274\n1187303285\n1164633233\n1153472231\n6643352233\n2643358322\n2243341322\n"),
    (10,"0481112976\n0031112009\n0041112504\n0081111406\n0099111306\n0093511233\n0442361130\n5532252350\n0532250600\n0032240000\n"),
    (20,"3936556452\n5686556806\n4496555690\n4448655580\n4456865570\n5680086577\n7000009896\n0000000344\n6000000364\n4600009543\n"),
    (100,"0397666866\n0749766918\n0053976933\n0004297822\n0004229892\n0053222877\n0532222966\n9322228966\n7922286866\n6789998766\n")
    ]

test :: ()
test
  | any testFail testData = error "a"
  | (fst . head . drop 10 . iterate step) start /= 204 = error "b"
  | (fst . head . drop 100 . iterate step) start /= 1656 = error "c"
  | otherwise = ()
  where
    start = (0,parse $ snd $ head testData)
    testFail (nsteps,result) =
        parse result /= (snd . head . drop nsteps . iterate step) start

part1 :: IO Int
part1 = fmap (fst . head . drop 100 . iterate step . (,) 0 . parse) $ readFile "input/11.txt"

findAllFlashStep :: Int -> Map (Int,Int) Int -> Int
findAllFlashStep nsteps ocs
  | all (== 0) ocs = nsteps
  | otherwise = findAllFlashStep (nsteps+1) (snd $ step (0,ocs))

test2 :: ()
test2
  | (findAllFlashStep 0 . parse) (snd $ head testData) /= 195 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (findAllFlashStep 0 . parse) $ readFile "input/11.txt"
