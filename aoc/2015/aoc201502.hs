import Data.List(sort)

parse :: String -> [Integer]
parse s = sort $ take 3 $ snd $ foldl p (0,[]) (s ++ "x")
  where
    p (acc,ls) '0' = (acc*10,ls)
    p (acc,ls) '1' = (acc*10+1,ls)
    p (acc,ls) '2' = (acc*10+2,ls)
    p (acc,ls) '3' = (acc*10+3,ls)
    p (acc,ls) '4' = (acc*10+4,ls)
    p (acc,ls) '5' = (acc*10+5,ls)
    p (acc,ls) '6' = (acc*10+6,ls)
    p (acc,ls) '7' = (acc*10+7,ls)
    p (acc,ls) '8' = (acc*10+8,ls)
    p (acc,ls) '9' = (acc*10+9,ls)
    p (acc,ls) _ = (0,acc:ls)

area :: String -> Integer
area s = 3*l*w + 2*w*h + 2*l*h
  where
    [l,w,h] = parse s

test :: ()
test
  | area "2x3x4" /= 58 = error "a"
  | area "1x1x10" /= 43 = error "b"
  | otherwise = ()

part1 :: IO Integer
part1 = fmap (sum . map area . words) (readFile "input/02.txt")

part2area :: String -> Integer
part2area s = 2*l+2*w+l*w*h
  where
    [l,w,h] = parse s

part2 :: IO Integer
part2 = fmap (sum . map part2area . words) (readFile "input/02.txt")
