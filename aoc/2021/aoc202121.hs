import Data.Map(Map,fromList,(!))

roll3x100 :: Int -> Int
roll3x100 rolls = (6 + rolls*3) `mod` 10

takeTurn :: (Int,(Int,Int)) -> (Int,(Int,Int))
takeTurn (score,(rolls,pos)) = (score+newpos,(rolls+6,newpos))
  where
    newpos = (pos - 1 + roll3x100 rolls) `mod` 10 + 1

init1000 :: (Int,Int) -> ((Int,(Int,Int)),(Int,(Int,Int)))
init1000 (pos1,pos2) = ((0,(0,pos1)),(0,(3,pos2)))

play1000 :: ((Int,(Int,Int)),(Int,(Int,Int))) ->  ((Int,(Int,Int)),(Int,(Int,Int)))
play1000 (p1,p2@(s2,_))
  | s2 >= 1000 = (p1,p2)
  | otherwise = play1000 (p2,takeTurn p1)

result1000 :: ((Int,(Int,Int)),(Int,(Int,Int))) -> Int
result1000 ((score,(rolls,_)),_) = rolls*score

test :: ()
test
  | (result1000 . play1000 . init1000) (4,8) /= 739785 = error "a"
  | otherwise = ()

part1 :: (Int,Int) -> Int
part1 = result1000 . play1000 . init1000

-- 3: (1x) 1 1 1 
-- 4: (3x) 1 1 2,1 2 1,2 1 1
-- 5: (6x) 1 1 3,1 3 1,3 1 1,1 2 2,2 1 2,2 2 1
-- 6: (7x) 1 2 3,1 3 2,2 1 3,2 3 1,3 1 2,3 2 1,2 2 2
-- 7: (6x) 3 3 1,3 1 3,1 3 3,3 2 2,2 3 2,2 2 3
-- 8: (3x) 3 3 2,3 2 3,2 3 3
-- 9: (1x) 3 3 3

outcomes :: Map (Bool,(Int,Int),(Int,Int)) (Integer,Integer)
outcomes = table
  where
    table = fromList [play (player,(pos1,score1),(pos2,score2)) | player <- [True,False], pos1 <- [1..10], score1 <- [0..20], pos2 <- [1..10], score2 <- [0..20]]

    (/*) a (x,y) = (x*a,y*a)
    (/+) (x1,y1) (x2,y2) = (x1+x2,y1+y2)
    play state@(True,(pos1,score1),p2) = (state,
           (1 /* (roll 3))
        /+ (3 /* (roll 4))
        /+ (6 /* (roll 5))
        /+ (7 /* (roll 6))
        /+ (6 /* (roll 7))
        /+ (3 /* (roll 8))
        /+ (1 /* (roll 9)))
      where
        roll n
          | newpos + score1 >= 21 = (1,0)
          | otherwise = table!(False,(newpos,score1+newpos),p2)
          where newpos = (pos1-1+n) `mod` 10 + 1
    play state@(False,p1,(pos2,score2)) = (state,
           (1 /* (roll 3))
        /+ (3 /* (roll 4))
        /+ (6 /* (roll 5))
        /+ (7 /* (roll 6))
        /+ (6 /* (roll 7))
        /+ (3 /* (roll 8))
        /+ (1 /* (roll 9)))
      where
        roll n
          | newpos + score2 >= 21 = (0,1)
          | otherwise = table!(True,p1,(newpos,score2+newpos))
          where newpos = (pos2-1+n) `mod` 10 + 1

run2 :: (Int,Int) -> (Integer,Integer)
run2 (pos1,pos2) = outcomes!(True,(pos1,0),(pos2,0))

test2 :: ()
test2
  | run2 (4,8) /= (444356092776315,341960390180808) = error "a"
  | otherwise = ()

part2 :: (Int,Int) -> Integer
part2 = uncurry max . run2
