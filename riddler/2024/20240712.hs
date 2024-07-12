choose :: Integer -> Integer -> Integer
choose n k = product [k+1..n] `div` product [1..n-k]

game :: Integer -> Rational
game p
  | p > 4 || p < 2 = 0
  | p == 4 = fromIntegral (choose 4 0)/2^4
  | p == 3 = fromIntegral (choose 5 1-choose 4 0)/2^5
  | p == 2 = 2*fromIntegral (choose 6 2-choose 5 1)/2^6

tiebreak :: Integer -> Rational
tiebreak p
  | p > 7 || p < 2 = 0
  | p == 7 = fromIntegral (choose 7 0)/2^7
  | p == 6 = fromIntegral (choose 8 1-choose 7 0)/2^8
  | p == 5 = fromIntegral (choose 9 2-choose 8 1)/2^9
  | p == 4 = fromIntegral (choose 10 3-choose 9 2)/2^10
  | p == 3 = fromIntegral (choose 11 4-choose 10 3)/2^11
  | p == 2 = 2*fromIntegral (choose 12 5-choose 11 4)/2^12

g :: Integer -> Rational
g 4 = 1/16
g 3 = 1/8
g 2 = 5/16
g _ = 0

t :: Integer -> Rational
t 7 = 1/128
t 6 = 7/256
t 5 = 7/128
t 4 = 21/256
t 3 = 105/1024
t 2 = 231/1024
t _ = 0

set :: Integer -> Rational
set p = s60 p + s61 p + s62 p + s63 p + s64 p + s75 p + s76 p

s60 :: Integer -> Rational
s60 p = fromIntegral (choose 5 5)
        *sum [g a
        *sum [g b
        *sum [g c
        *sum [g d
        *sum [g e
        *sum [g f
          | f <- [2..4], a+b+c+d+e+f == p]
          | e <- [2..4]]
          | d <- [2..4]]
          | c <- [2..4]]
          | b <- [2..4]]
          | a <- [2..4]]

s61 :: Integer -> Rational
s61 p = fromIntegral (choose 6 5)
        *sum [g a
        *sum [g b
        *sum [g c
        *sum [g d
        *sum [g e
        *sum [g f
        *sum [g z
          | z <- [2..4], p == a+b+c+d+e+f-z]
          | f <- [2..4]]
          | e <- [2..4]]
          | d <- [2..4]]
          | c <- [2..4]]
          | b <- [2..4]]
          | a <- [2..4]]

s62 :: Integer -> Rational
s62 p = fromIntegral (choose 7 5)
        *sum [g a
        *sum [g b
        *sum [g c
        *sum [g d
        *sum [g e
        *sum [g f
        *sum [g y
        *sum [g z
          | z <- [2..4], p == a+b+c+d+e+f-z-y]
          | y <- [2..4]]
          | f <- [2..4]]
          | e <- [2..4]]
          | d <- [2..4]]
          | c <- [2..4]]
          | b <- [2..4]]
          | a <- [2..4]]

s63 :: Integer -> Rational
s63 p = fromIntegral (choose 8 5)
        *sum [g a
        *sum [g b
        *sum [g c
        *sum [g d
        *sum [g e
        *sum [g f
        *sum [g x
        *sum [g y
        *sum [g z
          | z <- [2..4], p == a+b+c+d+e+f-z-y-x]
          | y <- [2..4]]
          | x <- [2..4]]
          | f <- [2..4]]
          | e <- [2..4]]
          | d <- [2..4]]
          | c <- [2..4]]
          | b <- [2..4]]
          | a <- [2..4]]

s64 :: Integer -> Rational
s64 p = fromIntegral (choose 9 5)
        *sum [g a
        *sum [g b
        *sum [g c
        *sum [g d
        *sum [g e
        *sum [g f
        *sum [g w
        *sum [g x
        *sum [g y
        *sum [g z
          | z <- [2..4], p == a+b+c+d+e+f-z-y-x-w]
          | y <- [2..4], p <= a+b+c+d+e+f-w-x-y-2 && p >= a+b+c+d+e+f-w-x-y-4]
          | x <- [2..4], p <= a+b+c+d+e+f-w-x-4 && p >= a+b+c+d+e+f-w-x-8]
          | w <- [2..4], p <= a+b+c+d+e+f-w-6 && p >= a+b+c+d+e+f-w-12]
          | f <- [2..4], p <= a+b+c+d+e+f-8 && p >= a+b+c+d+e+f-16]
          | e <- [2..4], p <= a+b+c+d+e+4-8 && p >= a+b+c+d+e+2-16]
          | d <- [2..4], p <= a+b+c+d+8-8 && p >= a+b+c+d+4-16]
          | c <- [2..4], p <= a+b+c+12-8 && p >= a+b+c+6-16]
          | b <- [2..4], p <= a+b+16-8 && p >= a+b+8-16]
          | a <- [2..4], p <= a+20-8 && p >= a+10-16]

s75 :: Integer -> Rational
s75 p = fromIntegral (choose 10 5*choose 2 2)
        *sum [g a
        *sum [g b
        *sum [g c
        *sum [g d
        *sum [g e
        *sum [g f
        *sum [g i
        *sum [g v
        *sum [g w
        *sum [g x
        *sum [g y
        *sum [g z
          | z <- [2..4], p == a+b+c+d+e+f+i-z-y-x-w-v]
          | y <- [2..4], p <= a+b+c+d+e+f+i-v-w-x-y-2 && p >= a+b+c+d+e+f+i-v-w-x-y-4]
          | x <- [2..4], p <= a+b+c+d+e+f+i-v-w-x-4 && p >= a+b+c+d+e+f+i-v-w-x-8]
          | w <- [2..4], p <= a+b+c+d+e+f+i-v-w-6 && p >= a+b+c+d+e+f+i-v-w-12]
          | v <- [2..4], p <= a+b+c+d+e+f+i-v-8 && p >= a+b+c+d+e+f+i-v-16]
          | i <- [2..4], p <= a+b+c+d+e+f+i-10 && p >= a+b+c+d+e+f+i-20]
          | f <- [2..4], p <= a+b+c+d+e+f+4-10 && p >= a+b+c+d+e+f+2-20]
          | e <- [2..4], p <= a+b+c+d+e+8-10 && p >= a+b+c+d+e+4-20]
          | d <- [2..4], p <= a+b+c+d+12-10 && p >= a+b+c+d+6-20]
          | c <- [2..4], p <= a+b+c+16-10 && p >= a+b+c+8-20]
          | b <- [2..4], p <= a+b+20-10 && p >= a+b+10-20]
          | a <- [2..4], p <= a+24-10 && p >= a+12-20]

s76 :: Integer -> Rational
s76 p = fromIntegral (choose 10 5*choose 2 1)
        *sum [t i
        *sum [g a
        *sum [g b
        *sum [g c
        *sum [g d
        *sum [g e
        *sum [g f
        *sum [g u
        *sum [g v
        *sum [g w
        *sum [g x
        *sum [g y
        *sum [g z
          | z <- [2..4], p == a+b+c+d+e+f-z-y-x-w-v-u+i]
          | y <- [2..4], p <= i+a+b+c+d+e+f-u-v-w-x-y-2 && p >= i+a+b+c+d+e+f-u-v-w-x-y-4]
          | x <- [2..4], p <= i+a+b+c+d+e+f-u-v-w-x-4 && p >= i+a+b+c+d+e+f-u-v-w-x-8]
          | w <- [2..4], p <= i+a+b+c+d+e+f-u-v-w-6 && p >= i+a+b+c+d+e+f-u-v-w-12]
          | v <- [2..4], p <= i+a+b+c+d+e+f-u-v-8 && p >= i+a+b+c+d+e+f-u-v-16]
          | u <- [2..4], p <= i+a+b+c+d+e+f-u-10 && p >= i+a+b+c+d+e+f-u-20]
          | f <- [2..4], p <= i+a+b+c+d+e+f-12 && p >= i+a+b+c+d+e+f-24]
          | e <- [2..4], p <= i+a+b+c+d+e+4-12 && p >= i+a+b+c+d+e+2-24]
          | d <- [2..4], p <= i+a+b+c+d+8-12 && p >= i+a+b+c+d+4-24]
          | c <- [2..4], p <= i+a+b+c+12-12 && p >= i+a+b+c+6-24]
          | b <- [2..4], p <= i+a+b+16-12 && p >= i+a+b+8-24]
          | a <- [2..4], p <= i+a+20-12 && p >= i+a+10-24]
          | i <- [2..7], p <= i+24-12 && p >= i+12-24]

s :: Integer -> Rational
s (-10) = 227390625/36028797018963968
s (-9) = 3377784375/36028797018963968
s (-8) = 3675459375/4503599627370496
s (-7) = 44568286875/9007199254740992
s (-6) = 418154223375/18014398509481984
s (-5) = 1580761885815/18014398509481984
s (-4) = 2506478162013/9007199254740992
s (-3) = 6822343581165/9007199254740992
s (-2) = 65659639352895/36028797018963968
s (-1) = 141855949498977/36028797018963968
s 0 = 69999234244471/9007199254740992
s 1 = 3928177031667/281474976710656
s 2 = 102533140105137/4503599627370496
s 3 = 298631549102215/9007199254740992
s 4 = 194441842388997/4503599627370496
s 5 = 224695658424321/4503599627370496
s 6 = 1889916128812943/36028797018963968
s 7 = 1817119418842665/36028797018963968
s 8 = 104013332049021/2251799813685248
s 9 = 361587464007267/9007199254740992
s 10 = 621851653240539/18014398509481984
s 11 = 511008190770639/18014398509481984
s 12 = 206711374130457/9007199254740992
s 13 = 157542640887153/9007199254740992
s 14 = 455881694219121/36028797018963968
s 15 = 297599471675615/36028797018963968
s 16 = 43599005810871/9007199254740992
s 17 = 11034802836897/4503599627370496
s 18 = 9638498680881/9007199254740992
s 19 = 1758991418679/4503599627370496
s 20 = 510765/4294967296
s 21 = 1933/67108864
s 22 = 735/134217728
s 23 = 3/4194304
s 24 = 1/16777216
s _ = 0

m :: Integer -> Rational
m p = sum [s q*s (p-q) | q <- [-10..24]]
    + 2*sum [sum [s q*s r*s (p-q+r) | r <- [-10..24]] | q <- [-10..24]]

main :: IO ()
main = print (result,fromRational result)
  where
    result = 2*sum [m p | p <- [-44 .. -1]]
