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
