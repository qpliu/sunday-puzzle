increment :: String -> String
increment s = fst $ foldr inc ("",True) s
  where
    inc 'i' (p,_) = ('j':map (const 'a') p,False)
    inc 'o' (p,_) = ('p':map (const 'a') p,False)
    inc 'l' (p,_) = ('m':map (const 'a') p,False)
    inc c (p,False) = (c:p,False)
    inc 'z' (p,True) = ('a':p,True)
    inc c (p,True) = (succ c:p,False)

hasStraight :: String -> Bool
hasStraight (a:b:c:_) | succ a == b && succ b == c = True
hasStraight (a:as) = hasStraight as
hasStraight [] = False

hasNoIOL :: String -> Bool
hasNoIOL s = not $ any (`elem` "iol") s

has2Pairs :: String -> Bool
has2Pairs s = countPairs s 0 >= 2
  where
    countPairs (a:as@(b:bs)) n
      | a == b = countPairs bs (n+1)
      | otherwise = countPairs as n
    countPairs _ n = n

next :: String -> String
next p = head $ filter has2Pairs $ filter hasNoIOL $ filter hasStraight $ drop 1 $ iterate increment p

test :: ()
test
  | not (hasStraight "hijklmmn") = error "a"
  | hasNoIOL "hijklmmn" = error "b"
  | hasStraight "abbceffg" = error "c"
  | not (has2Pairs "abbceffg") = error "d"
  | has2Pairs "abbcegjk" = error "e"
  | next "abcdefgh" /= "abcdffaa" = error "f"
  | next "ghijklmn" /= "ghjaabcc" = error "g"
  | otherwise = ()

part1 :: String -> String
part1 = next

part2 :: String -> String
part2 = next . next
