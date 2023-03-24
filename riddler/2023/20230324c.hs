p :: Bool -> Int -> Int -> Double
p extra a b
  | extra = 0.5 + 0.033*fromIntegral (b - a)
  | otherwise = 0.5

pgroup :: Bool -> Int -> Int -> Int -> Int -> Double
pgroup extra s1 s16 s8 s9 =
    p1*p16_8_9 + p16*p1_8_9 + p8*p1_16_9 + p9*p1_16_8
  where
    p1 = p extra s1 s16*(p extra s1 s8*p extra s8 s9 + p extra s1 s9*p extra s9 s8)
    p16 = p extra s16 s1*(p extra s16 s8*p extra s8 s9 + p extra s16 s9*p extra s9 s8)
    p8 = p extra s8 s9*(p extra s8 s1*p extra s1 s16 + p extra s8 s16*p extra s16 s1)
    p9 = p extra s9 s8*(p extra s9 s1*p extra s1 s16 + p extra s9 s16*p extra s16 s1)

    p16_8_9 = p16*p8_9 + p8*p16_9 + p9*p16_8
    p1_8_9 = p1*p8_9 + p8*p1_9 + p9*p1_8
    p1_16_9 = p1*p16_9 + p16*p1_9 + p9*p1_16
    p1_16_8 = p1*p16_8 + p16*p1_8 + p8*p1_16

    p8_9 = 2*p8*p9
    p16_9 = 2*p16*p9
    p16_8 = 2*p16*p8
    p1_9 = 2*p1*p9
    p1_8 = 2*p1*p8
    p1_16 = 2*p1*p16

psweet16 :: Bool -> Double
psweet16 extra =
    pgroup extra 1 16 8 9
  * pgroup extra 2 15 7 10
  * pgroup extra 3 14 6 11
  * pgroup extra 4 13 5 12

main :: IO ()
main = do
    print (psweet16 False)
    print (psweet16 True)
