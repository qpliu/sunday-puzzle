-- pk1 = a^k1 `mod` b
-- pk2 = a^k2 `mod` b
-- k = a^(k1*k2) `mod` b

powMod :: Int -> Int -> Int -> Int
powMod x y m = f (x `rem` m) y 1 `mod` m
  where
    f _ 0 acc = acc
    f b e acc = f (b * b `rem` m) (e `quot` 2)
      (if odd e then b * acc `rem` m else acc)

encryptionKey :: Int -> Int -> Int
encryptionKey pubKey1 pubKey2 = search (7,1)
  where
    search (k,n)
      | k == pubKey1 = powMod pubKey2 n 20201227
      | k == pubKey2 = powMod pubKey1 n 20201227
      | otherwise = search (k*7 `mod` 20201227,n+1)

test :: ()
test
  | encryptionKey 5764801 17807724 /= 14897079 = error "a"
  | otherwise = ()

part1 :: Int -> Int -> Int
part1 = encryptionKey
