import Data.Bits(clearBit,setBit,testBit)
import Data.Char(isDigit)
import Data.Map(Map,empty,insert)
import Data.Set(Set,difference,elems,fromList,intersection,member,singleton,size,union)

parse :: String -> [(([Int],[Int]),(Int,Int))]
parse = p ([],[]) . words
  where
    p _ ("mask":"=":mask:rest) = p (parseMask mask) rest
    p mask (mem:"=":val:rest) = (mask,(read (filter isDigit mem),read val)) : p mask rest
    p _ _ = []
    parseMask mask = ((map fst . filter ((== '1') . snd) . zip [35,34..0]) mask,(map fst . filter ((== '0') . snd) . zip [35,34..0]) mask)

exec1 :: Map Int Int -> (([Int],[Int]),(Int,Int)) -> Map Int Int
exec1 mem ((mask1,mask0),(loc,val)) = insert loc maskedVal mem
  where
    maskedVal = foldl setBit (foldl clearBit val mask0) mask1

exec :: Map Int Int -> [(([Int],[Int]),(Int,Int))] -> Map Int Int
exec = foldl exec1

testData :: String
testData = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0\n"

test :: ()
test
  | (sum . exec empty . parse) testData /= 165 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . exec empty . parse) $ readFile "input/14.txt"

-- For part 2, need to remove the subsequently overwritten locations
-- from the counts for each write.

type Write2 = (Set Int,Set Int,Set Int,Int)
-- (addr one bits,addr zero bits,addr floating bits,value)

toWrite2 :: (([Int],[Int]),(Int,Int)) -> Write2
toWrite2 ((mask1,mask0),(loc,val)) = (ones,zeros,floating,val)
  where
    ones = fromList $ mask1 ++ filter (testBit loc) mask0
    zeros = fromList $ filter (not . testBit loc) mask0
    floating = fromList [b | b <- [0..35], not (member b ones), not (member b zeros)]

removeOverwritten :: [Write2] -> [Write2]
removeOverwritten [] = []
removeOverwritten (w:ws) = foldl removeOverwritten1 [w] ws ++ removeOverwritten ws
  where rest = removeOverwritten ws

removeOverwritten1 :: [Write2] -> Write2 -> [Write2]
removeOverwritten1 w1s w2 = concatMap (overwrite w2) w1s

overwrite :: Write2 -> Write2 -> [Write2]
overwrite w2@(o2,z2,f2,_) w1@(o1,z1,f1,v1)
  | size (intersection o1 z2) /= 0 = [w1]
  | size (intersection z1 o2) /= 0 = [w1]
  | size (difference f1 f2) == 0 = []
  | otherwise = rm fMustBeO fMustBeZ w1
  where
    -- At least one of these floating bits must be different from w2
    fMustBeO = map singleton $ elems $ intersection f1 z2
    fMustBeZ = map singleton $ elems $ intersection f1 o2
    -- In the first, set the first floating bit different from w2,
    -- and leave the rest of the floating bits floating.
    -- For the rest, set the first floating bit the same as w2,
    -- recurse for the rest of the floating bits.
    rm (f2o:f2os) f2zs (o,z,f,v) = wf2o : rm f2os f2zs wf2oRest
      where wf2o = (union o f2o,z,difference f f2o,v)
            wf2oRest = (o,union z f2o,difference f f2o,v)
    rm [] (f2z:f2zs) (o,z,f,v) = wf2z : rm [] f2zs wf2zRest
      where wf2z = (o,union z f2z,difference f f2z,v)
            wf2zRest = (union o f2z,z,difference f f2z,v)
    rm [] [] _ = []
    
sumWrites :: Write2 -> Int
sumWrites (_,_,floating,val) = val * 2^(size floating)

testData2 :: String
testData2 = "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1\n"

test2 :: ()
test2
  | (sum . map sumWrites . removeOverwritten . map toWrite2 . parse) testData2 /= 208 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (sum . map sumWrites . removeOverwritten . map toWrite2 . parse) $ readFile "input/14.txt"
