module AOC201522 where

import AOC

aoc = AOC {
    day="../../2015/input/22",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Hit Points: 13",
                "Damage: 8"
                ],
            testResult=Just "226",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 10 250,
        codeTest2=result2 10 250,
        codeResult=result 50 500,
        codeResult2=result2 50 500
        }
    }

parse = parseInts

search :: (Int -> Int) -> Int -> Int -> [Int] -> Int
search difficulty initialHP initialMana [initialBossHP,bossDamage] = finalCost
  where
    Just (finalCost,_) = astar heuristic neighbors toState done initialPaths

    initialPaths = [(0,(difficulty initialHP,initialMana,initialBossHP,0,0,0))]
    done (_,(_,_,bossHP,_,_,_)) = bossHP <= 0
    toState = snd
    heuristic = fst

    neighbors (cost,(hp,mana,bossHP,shield,poison,recharge))
      | hp <= 0 = []
      | otherwise = castRecharge
      where
        castRecharge
          | recharge > 0 || mana < 229 = castPoison
          | otherwise =
              (effects difficulty $ boss $ effects id doRecharge) : castPoison
        castPoison
          | poison > 0 || mana < 173 = castShield
          | otherwise =
              (effects difficulty $ boss $ effects id doPoison) : castShield
        castShield
          | shield > 0 || mana < 113 = castDrain
          | otherwise =
              (effects difficulty $ boss $ effects id doShield) : castDrain
        castDrain
          | mana < 73 = castMM
          | otherwise =
              (effects difficulty $ boss $ effects id doDrain) : castMM
        castMM
          | mana < 53 = []
          | otherwise = [effects difficulty $ boss $ effects id doMagicMissile]

        doRecharge = (cost+229,(hp,mana-229,bossHP,shield,poison,5))
        doPoison = (cost+173,(hp,mana-173,bossHP,shield,6,recharge))
        doShield = (cost+113,(hp,mana-113,bossHP,6,poison,recharge))
        doDrain = (cost+73,(hp+2,mana-73,bossHP-2,shield,poison,recharge))
        doMagicMissile = (cost+53,(hp,mana-53,bossHP-4,shield,poison,recharge))

    effects difficulty (cost,(hp,mana,bossHP,shield,poison,recharge)) =
        (cost,(difficulty hp,
               mana+if recharge > 0 then 101 else 0,
               bossHP-if poison > 0 then 3 else 0,
               max 0 (shield-1), max 0 (poison-1), max 0 (recharge-1)))
    boss (cost,(hp,mana,bossHP,shield,poison,recharge)) =
        (cost,(hp-max 1 (bossDamage - if shield > 0 then 7 else 0),
               mana,bossHP,shield,poison,recharge))

result = search id

result2 = search pred
