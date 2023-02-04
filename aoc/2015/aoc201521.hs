import Data.List(sort)

weapons :: [(Int,(Int,Int))]
weapons = [(8,(4,0)),(10,(5,0)),(25,(6,0)),(40,(7,0)),(74,(8,0))]

armor :: [(Int,(Int,Int))]
armor = [(0,(0,0)),(13,(0,1)),(31,(0,2)),(53,(0,3)),(75,(0,4)),(102,(0,5))]

rings :: [(Int,(Int,Int))]
rings = (0,(0,0)) : [add r1 r2 | r1 <- wares, r2 <- wares, r1 > r2]
  where
    wares = [(0,(0,0)),(25,(1,0)),(50,(2,0)),(100,(3,0)),(20,(0,1)),(40,(0,2)),(80,(0,3))]

add :: (Int,(Int,Int)) -> (Int,(Int,Int)) -> (Int,(Int,Int))
add (c1,(d1,a1)) (c2,(d2,a2)) = (c1+c2,(d1+d2,a1+a2))

gear :: [(Int,(Int,Int))]
gear = sort [add w (add a r) | w <- weapons, a <- armor, r <- rings]

winBattle :: (Int,Int,Int) -> Int -> (Int,(Int,Int)) -> Bool
winBattle (bossHP,bossDamage,bossArmor) playerHP (_,(playerDamage,playerArmor)) = turns bossDamage playerArmor playerHP >= turns playerDamage bossArmor bossHP
  where
    turns damage armor hp = (hp+netDamage-1) `div` netDamage
      where netDamage = max 1 (damage - armor)

winningGear :: Int -> (Int,Int,Int) -> [(Int,(Int,Int))]
winningGear playerHP boss = filter (winBattle boss playerHP) gear

test :: ()
test | not $ winBattle (12,7,2) 8 (0,(5,5)) = error "a"
     | otherwise = ()

part1 :: (Int,Int,Int) -> Int
part1 boss = fst $ head $ winningGear 100 boss

losingGear :: Int -> (Int,Int,Int) -> [(Int,(Int,Int))]
losingGear playerHP boss = filter (not . winBattle boss playerHP) gear

part2 boss = fst $ last $ losingGear 100 boss
