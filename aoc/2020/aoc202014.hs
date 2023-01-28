{-
--- Day 14: Docking Data ---

As your ferry approaches the sea port, the captain asks for your help again.
The computer system that runs this port isn't compatible with the docking
program on the ferry, so the docking parameters aren't being correctly
initialized in the docking program's memory.

After a brief inspection, you discover that the sea port's computer system uses
a strange bitmask system in its initialization program. Although you don't have
the correct decoder chip handy, you can emulate it in software!

The initialization program (your puzzle input) can either update the bitmask or
write a value to memory. Values and memory addresses are both 36-bit unsigned
integers. For example, ignoring bitmasks for a moment, a line like mem[8] = 11
would write the value 11 to memory address 8.

The bitmask is always given as a string of 36 bits, written with the most
significant bit (representing 2^35) on the left and the least significant bit
(2^0, that is, the 1s bit) on the right. The current bitmask is applied to
values immediately before they are written to memory: a 0 or 1 overwrites the
corresponding bit in the value, while an X leaves the bit in the value
unchanged.

For example, consider the following program:

| mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
| mem[8] = 11
| mem[7] = 101
| mem[8] = 0

This program starts by specifying a bitmask (mask = ....). The mask it
specifies will overwrite two bits in every written value: the 2s bit is
overwritten with 0, and the 64s bit is overwritten with 1.

The program then attempts to write the value 11 to memory address 8. By
expanding everything out to individual bits, the mask is applied as follows:

| value:  000000000000000000000000000000001011  (decimal 11)
| mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
| result: 000000000000000000000000000001001001  (decimal 73)

So, because of the mask, the value 73 is written to memory address 8 instead.
Then, the program tries to write 101 to address 7:

| value:  000000000000000000000000000001100101  (decimal 101)
| mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
| result: 000000000000000000000000000001100101  (decimal 101)

This time, the mask has no effect, as the bits it overwrote were already the
values the mask tried to set. Finally, the program tries to write 0 to address
8:

| value:  000000000000000000000000000000000000  (decimal 0)
| mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
| result: 000000000000000000000000000001000000  (decimal 64)

64 is written to address 8 instead, overwriting the value that was there
previously.

To initialize your ferry's docking program, you need the sum of all values left
in memory after the initialization program completes. (The entire 36-bit
address space begins initialized to the value 0 at every address.) In the
above example, only two values in memory are not zero - 101 (at address 7) and
64 (at address 8) - producing a sum of 165.

Execute the initialization program. What is the sum of all values left in
memory after it completes? (Do not truncate the sum to 36 bits.)

--- Part Two ---

For some reason, the sea port's computer system still can't communicate with
your ferry's docking program. It must be using version 2 of the decoder chip!

A version 2 decoder chip doesn't modify the values being written at all.
Instead, it acts as a memory address decoder. Immediately before a value is
written to memory, each bit in the bitmask modifies the corresponding bit of
the destination memory address in the following way:

 - If the bitmask bit is 0, the corresponding memory address bit is unchanged.
 - If the bitmask bit is 1, the corresponding memory address bit is overwritten
   with 1.
 - If the bitmask bit is X, the corresponding memory address bit is floating.

A floating bit is not connected to anything and instead fluctuates
unpredictably. In practice, this means the floating bits will take on all
possible values, potentially causing many memory addresses to be written all
at once!

For example, consider the following program:

| mask = 000000000000000000000000000000X1001X
| mem[42] = 100
| mask = 00000000000000000000000000000000X0XX
| mem[26] = 1

When this program goes to write to memory address 42, it first applies the
bitmask:

| address: 000000000000000000000000000000101010  (decimal 42)
| mask:    000000000000000000000000000000X1001X
| result:  000000000000000000000000000000X1101X

After applying the mask, four bits are overwritten, three of which are
different, and two of which are floating. Floating bits take on every possible
combination of values; with two floating bits, four actual memory addresses are
written:

| 000000000000000000000000000000011010  (decimal 26)
| 000000000000000000000000000000011011  (decimal 27)
| 000000000000000000000000000000111010  (decimal 58)
| 000000000000000000000000000000111011  (decimal 59)

Next, the program is about to write to memory address 26 with a different
bitmask:

| address: 000000000000000000000000000000011010  (decimal 26)
| mask:    00000000000000000000000000000000X0XX
| result:  00000000000000000000000000000001X0XX

This results in an address with three floating bits, causing writes to eight
memory addresses:

| 000000000000000000000000000000010000  (decimal 16)
| 000000000000000000000000000000010001  (decimal 17)
| 000000000000000000000000000000010010  (decimal 18)
| 000000000000000000000000000000010011  (decimal 19)
| 000000000000000000000000000000011000  (decimal 24)
| 000000000000000000000000000000011001  (decimal 25)
| 000000000000000000000000000000011010  (decimal 26)
| 000000000000000000000000000000011011  (decimal 27)

The entire 36-bit address space still begins initialized to the value 0 at
every address, and you still need the sum of all values left in memory at the
end of the program. In this example, the sum is 208.

Execute the initialization program using an emulator for a version 2 decoder
chip. What is the sum of all values left in memory after it completes?
-}

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
