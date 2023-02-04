import Data.Set(fromList,member)

parse :: String -> [Int]
parse = map (sum . zipWith (*) [2^n | n <- [9,8..0]] . map parseChar) . words
  where
    parseChar 'B' = 1
    parseChar 'R' = 1
    parseChar _ = 0

test :: ()
test
  | parse "FBFBBFFRLR" /= [357] = error "a"
  | parse "BFFFBBFRRR" /= [567] = error "b"
  | parse "FFFBBBFRRR" /= [119] = error "b"
  | parse "BBFFBBFRLL" /= [820] = error "b"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (maximum . parse) $ readFile "input/05.txt"

findEmpty :: [Int] -> [Int]
findEmpty ticketList = [tid | tid <- [minimum tickets .. maximum tickets], member (tid+1) tickets, member (tid-1) tickets, not (member tid tickets)]
  where
    tickets = fromList ticketList

part2 :: IO [Int]
part2 = fmap (findEmpty . parse) $ readFile "input/05.txt"
