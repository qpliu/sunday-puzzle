import Data.Bits((.&.),(.|.))
import Data.Set(Set,empty,member,insert)

-- Decompiling the main loop in my input:

insn6 (r2,r3) = insn8 (r3 .|. 65536, 832312)

insn8 (r2,r3) = insn13 (r2,(((r3 + (r2 .&. 255)) .&. 16777215) * 65899) .&. 16777215)

insn13 (r2,r3)
  | r2 < 256 = insn28 (r2,r3)
  | otherwise = insn8 (r2 `div` 256,r3)

insn28 = id
-- insn28 (r0,r2,r3) | r0 /= r3 = insn6 (r0,r2,r3) | otherwise = terminate

part1 :: Int
part1 = snd $ insn6 (0,0)

-- Find cycle in the loop.
-- For my input, there are 11547 unique states.
uniques :: Ord a => [a] -> [a]
uniques as = scan empty as
  where
    scan set (a:as)
      | a `member` set = []
      | otherwise = a : scan (insert a set) as

unique :: Ord a => [a] -> [a]
unique as = scan empty as
  where
    scan set [] = []
    scan set (a:as)
      | a `member` set = scan set as
      | otherwise = a : scan (insert a set) as

part2 :: Int
part2 = last $ unique $ map snd $ uniques $ iterate insn6 (0,0)
