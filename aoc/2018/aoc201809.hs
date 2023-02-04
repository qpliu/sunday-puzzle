import Data.Array(Array,array,(!),(//))

type State = (Int,[Int],([Int],[Int])) -- next marble, circle, scores

initState :: Int -> State
initState numPlayers = (1,[0],(take numPlayers $ repeat 0,[]))

turn :: State -> State
turn state@(nextMarble,circle,(playersYetToPlay,playersPlayed))
  | null playersYetToPlay = turn (nextMarble,circle,(reverse playersPlayed,[]))
  | nextMarble `mod` 23 /= 0 = (nextMarble+1,nextMarble:circle2++circle0,(tail playersYetToPlay,head playersYetToPlay:playersPlayed))
  | otherwise = (nextMarble+1,circle6++circle8,(tail playersYetToPlay,(head playersYetToPlay + m7 + nextMarble):playersPlayed))
  where
    (circle0,circle2) = splitAt 2 circle
    (circle8,m7:circle6) = splitAt (length circle - 7) circle

scores :: State -> [Int]
scores (_,_,(playersYetToPlay,playersPlayed)) = reverse playersPlayed ++ playersYetToPlay

play :: Int -> Int -> [Int]
play numPlayers lastMarble = scores $ head $ drop lastMarble $ iterate turn (initState numPlayers)

test :: ()
test
  | maximum (play 9 25) /= 32 = error "a"
  | maximum (play 10 1618) /= 8317 = error "b"
  | maximum (play 13 7999) /= 146373 = error "c"
  | maximum (play 17 1104) /= 2764 = error "d"
  | maximum (play 21 6111) /= 54718 = error "e"
  | maximum (play 30 5807) /= 37305 = error "f"
  | otherwise = ()

-- this is super slow
part1 :: Int -> Int -> Int
part1 numPlayers lastMarble = maximum $ play numPlayers lastMarble

-- much faster, but still scales just as poorly
scoring2 :: [(Int,Int)]
scoring2 = (23,9):s 46 [17,42,4,43,18,44,19,45,2,24,20,25,10,26,21,27,5,28,22,29,11,30,1,31,12,32,6,33,13,34,3,35,14,36,7,37,15,38,0,39,16,40,8,41]
  where
    s m (m1:m2:m3:m4:m5:m6:m7:m8:m9:m10:m11:m12:m13:m14:m15:m16:m17:m18:m19:m20:m21:m22:m23:m24:rest) = (m,m1):s (m+23) (m21:m+19:m22:m+20:m23:m+21:m24:m+22:rest ++ [m2,m3,m+1,m4,m+2,m5,m+3,m6,m+4,m7,m+5,m8,m+6,m9,m+7,m10,m+8,m11,m+9,m12,m+10,m13,m+11,m14,m+12,m15,m+13,m16,m+14,m17,m+15,m18,m+16,m19,m+17,m20,m+18])

scores2 :: Int -> [(Int,(Int,Array Int Int))]
scores2 nplayers = s scoring2 $ array (0,nplayers-1) $ zip [0..nplayers-1] $ repeat 0
  where
    s ((n,m):rest) players = (n,(m,newplayers)) : s rest newplayers
      where p = n `mod` nplayers
            newplayers = players // [(p,players!p + n + m)]

part2 :: Int -> Int -> Int
part2 nplayers lastMarble = maximum $ snd $ snd $ head $ dropWhile ((< (lastMarble-22)) . fst) $ scores2 nplayers
