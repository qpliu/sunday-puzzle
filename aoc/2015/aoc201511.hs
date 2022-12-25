{-
--- Day 11: Corporate Policy ---

Santa's previous password expired, and he needs help choosing a new one.

To help him remember his new password after the old one expires, Santa has
devised a method of coming up with a password based on the previous one.
Corporate policy dictates that passwords must be exactly eight lowercase
letters (for security reasons), so he finds his new password by incrementing
his old password string repeatedly until it is valid.

Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so on.
Increase the rightmost letter one step; if it was z, it wraps around to a, and
repeat with the next letter to the left until one doesn't wrap around.

Unfortunately for Santa, a new Security-Elf recently started, and he has
imposed some additional password requirements:

 - Passwords must include one increasing straight of at least three letters,
   like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd
   doesn't count.
 - Passwords may not contain the letters i, o, or l, as these letters can be
   mistaken for other characters and are therefore confusing.
 - Passwords must contain at least two different, non-overlapping pairs of
   letters, like aa, bb, or zz.

For example:

 - hijklmmn meets the first requirement (because it contains the straight hij)
   but fails the second requirement requirement (because it contains i and l).
 - abbceffg meets the third requirement (because it repeats bb and ff) but
   fails the first requirement.
 - abbcegjk fails the third requirement, because it only has one double letter
   (bb).
 - The next password after abcdefgh is abcdffaa.
 - The next password after ghijklmn is ghjaabcc, because you eventually skip
   all the passwords that start with ghi..., since i is not allowed.

Given Santa's current password (your puzzle input), what should his next password be?
-}

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
