{-
--- Day 7: Internet Protocol Version 7 ---

While snooping around the local network of EBHQ, you compile a list of IP
addresses (they're IPv7, of course; IPv6 is much too limited). You'd like to
figure out which IPs support TLS (transport-layer snooping).

An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA.
An ABBA is any four-character sequence which consists of a pair of two
different characters followed by the reverse of that pair, such as xyyx or
abba. However, the IP also must not have an ABBA within any hypernet sequences,
which are contained by square brackets.

For example:

 - abba[mnop]qrst supports TLS (abba outside square brackets).
 - abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even
   though xyyx is outside square brackets).
 - aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior
   characters must be different).
 - ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even
   though it's within a larger string).

How many IPs in your puzzle input support TLS?
-}

import Data.Set(fromList,intersection,size)

supportsTLS :: String -> Bool
supportsTLS s = scan False 0 s
  where
    scan hasABBA nesting "" = hasABBA
    scan hasABBA nesting (a:b:c:d:e)
      | a == d && b == c && a /= b && a /= '[' && a /= ']' && b /= '[' && b /= ']' = if nesting > 0 then False else scan True nesting e
    scan hasABBA nesting (c:cs)
      | c == '[' = scan hasABBA (nesting+1) cs
      | c == ']' = scan hasABBA (nesting-1) cs
      | otherwise = scan hasABBA nesting cs

test :: ()
test
  | not (supportsTLS "abba[mnop]qrst") = error "a"
  | supportsTLS "abcd[bddb]xyyx" = error "b"
  | supportsTLS "aaaa[qwer]tyui" = error "c"
  | not (supportsTLS "ioxxoj[asdfgh]zxcvbn") = error "d"
  | otherwise = ()


part1 :: IO Int
part1 = fmap (length . filter supportsTLS . lines) $ readFile "input/07.txt"

supportsSSL :: String -> Bool
supportsSSL s = size (fromList aba `intersection` fromList bab) > 0
  where
    (aba,bab) = scan ([],[]) 0 s
    scan (abas,babs) nesting "" = (abas,babs)
    scan (abas,babs) nesting ('[':rest) = scan (abas,babs) (nesting+1) rest
    scan (abas,babs) nesting (']':rest) = scan (abas,babs) (nesting-1) rest
    scan (abas,babs) nesting (a:rest@(b:c:_))
      | nesting == 0 && a == c && a /= b = scan ((a,b):abas,babs) nesting rest
      | nesting > 0 && a == c && a /= b = scan (abas,(b,a):babs) nesting rest
    scan (abas,babs) nesting (_:rest) = scan (abas,babs) nesting rest

test2 :: ()
test2
  | not (supportsSSL "aba[bab]xyz") = error "a"
  | supportsSSL "xyx[xyx]xyx" = error "b"
  | not (supportsSSL "aaa[kek]eke") = error "c"
  | not (supportsSSL "zazbz[bzb]cdb") = error "d"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (length . filter supportsSSL . lines) $ readFile "input/07.txt"
