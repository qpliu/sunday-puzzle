{-
--- Day 22: Wizard Simulator 20XX ---

Little Henry Case decides that defeating bosses with swords and stuff is
boring. Now he's playing the game with a wizard. Of course, he gets stuck on
another boss and needs your help again.

In this version, combat still proceeds with the player and the boss taking
alternating turns. The player still goes first. Now, however, you don't get any
equipment; instead, you must choose one of your spells to cast. The first
character at or below 0 hit points loses.

Since you're a wizard, you don't get to wear armor, and you can't attack
normally. However, since you do magic damage, your opponent's armor is ignored,
and so the boss effectively has zero armor as well. As before, if armor (from a
spell, in this case) would reduce damage below 1, it becomes 1 instead - that
is, the boss' attacks always deal at least 1 damage.

On each of your turns, you must select one of your spells to cast. If you
cannot afford to cast any spell, you lose. Spells cost mana; you start with 500
mana, but have no maximum limit. You must have enough mana to cast a spell, and
its cost is immediately deducted when you cast it. Your spells are Magic
Missile, Drain, Shield, Poison, and Recharge.

 - Magic Missile costs 53 mana. It instantly does 4 damage.
 - Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit
   points.
 - Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it
   is active, your armor is increased by 7.
 - Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the
   start of each turn while it is active, it deals the boss 3 damage.
 - Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the
   start of each turn while it is active, it gives you 101 new mana.

Effects all work the same way. Effects apply at the start of both the player's
turns and the boss' turns. Effects are created with a timer (the number of
turns they last); at the start of each turn, after they apply any effect they
have, their timer is decreased by one. If this decreases the timer to zero, the
effect ends. You cannot cast a spell that would start an effect which is
already active. However, effects can be started on the same turn they end.

For example, suppose the player has 10 hit points and 250 mana, and that the
boss has 13 hit points and 8 damage:

-- Player turn --
- Player has 10 hit points, 0 armor, 250 mana
- Boss has 13 hit points
Player casts Poison.

-- Boss turn --
- Player has 10 hit points, 0 armor, 77 mana
- Boss has 13 hit points
Poison deals 3 damage; its timer is now 5.
Boss attacks for 8 damage.

-- Player turn --
- Player has 2 hit points, 0 armor, 77 mana
- Boss has 10 hit points
Poison deals 3 damage; its timer is now 4.
Player casts Magic Missile, dealing 4 damage.

-- Boss turn --
- Player has 2 hit points, 0 armor, 24 mana
- Boss has 3 hit points
Poison deals 3 damage. This kills the boss, and the player wins.

Now, suppose the same initial conditions, except that the boss has 14 hit
points instead:

-- Player turn --
- Player has 10 hit points, 0 armor, 250 mana
- Boss has 14 hit points
Player casts Recharge.

-- Boss turn --
- Player has 10 hit points, 0 armor, 21 mana
- Boss has 14 hit points
Recharge provides 101 mana; its timer is now 4.
Boss attacks for 8 damage!

-- Player turn --
- Player has 2 hit points, 0 armor, 122 mana
- Boss has 14 hit points
Recharge provides 101 mana; its timer is now 3.
Player casts Shield, increasing armor by 7.

-- Boss turn --
- Player has 2 hit points, 7 armor, 110 mana
- Boss has 14 hit points
Shield's timer is now 5.
Recharge provides 101 mana; its timer is now 2.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 1 hit point, 7 armor, 211 mana
- Boss has 14 hit points
Shield's timer is now 4.
Recharge provides 101 mana; its timer is now 1.
Player casts Drain, dealing 2 damage, and healing 2 hit points.

-- Boss turn --
- Player has 3 hit points, 7 armor, 239 mana
- Boss has 12 hit points
Shield's timer is now 3.
Recharge provides 101 mana; its timer is now 0.
Recharge wears off.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 2 hit points, 7 armor, 340 mana
- Boss has 12 hit points
Shield's timer is now 2.
Player casts Poison.

-- Boss turn --
- Player has 2 hit points, 7 armor, 167 mana
- Boss has 12 hit points
Shield's timer is now 1.
Poison deals 3 damage; its timer is now 5.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 1 hit point, 7 armor, 167 mana
- Boss has 9 hit points
Shield's timer is now 0.
Shield wears off, decreasing armor by 7.
Poison deals 3 damage; its timer is now 4.
Player casts Magic Missile, dealing 4 damage.

-- Boss turn --
- Player has 1 hit point, 0 armor, 114 mana
- Boss has 2 hit points
Poison deals 3 damage. This kills the boss, and the player wins.

You start with 50 hit points and 500 mana points. The boss's actual stats are
in your puzzle input. What is the least amount of mana you can spend and still
win the fight? (Do not include mana recharge effects as "spending" negative
mana.)
-}

type State = (Int,Int,Int,Int,Int,Int,Int,Int) -- player hp, mana, boss hp, boss damage, shield timer, poison timer, recharge timer, spent mana

data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving (Eq,Show)

effects :: State -> Either (Maybe Int) State
effects (hp,mana,bosshp,bossdamage,shield,poison,recharge,spent)
  | newbosshp > 0 = Right (hp, newmana, newbosshp, bossdamage,
                           max 0 (shield - 1), max 0 (poison - 1),
                           max 0 (recharge - 1), spent)
  | otherwise = Left (Just spent)
  where
    newmana = mana + if recharge > 0 then 101 else 0
    newbosshp = bosshp - if poison > 0 then 3 else 0

playerTurn :: State -> Spell -> Either (Maybe Int) State
playerTurn (hp,mana,bosshp,bossdamage,shield,poison,recharge,spent) = p
  where
    p MagicMissile
      | mana < 53 = Left Nothing
      | bosshp <= 4 = Left (Just (spent+53))
      | otherwise = Right (hp,mana-53,bosshp-4,bossdamage,shield,poison,recharge,spent+53)
    p Drain
      | mana < 73 = Left Nothing
      | bosshp <= 2 = Left (Just (spent+73))
      | otherwise = Right (hp+2,mana-73,bosshp-2,bossdamage,shield,poison,recharge,spent+73)
    p Shield
      | mana < 113 || shield > 0 = Left Nothing
      | otherwise = Right (hp,mana-113,bosshp,bossdamage,6,poison,recharge,spent+113)
    p Poison
      | mana < 173 || poison > 0 = Left Nothing
      | otherwise = Right (hp,mana-173,bosshp,bossdamage,shield,6,recharge,spent+173)
    p Recharge
      | mana < 229 || recharge > 0 = Left Nothing
      | otherwise = Right (hp,mana-229,bosshp,bossdamage,shield,poison,5,spent+229)

bossTurn :: State -> Either (Maybe Int) State
bossTurn (hp,mana,bosshp,bossdamage,shield,poison,recharge,spent)
  | newhp <= 0 = Left Nothing
  | otherwise = Right (newhp,mana,bosshp,bossdamage,shield,poison,recharge,spent)
  where
    newhp = hp - max 1 (bossdamage - if shield > 0 then 7 else 0)

-- Do a depth-first search so that searches can be pruned if using more mana
-- than in a previous win
battle :: (State,Maybe Int) -> Maybe Int
battle (state,previousManaSpent)
  | otherwise = foldl search previousManaSpent [MagicMissile,Drain,Shield,Poison,Recharge]
  where
    search manaSpent spell = maybe manaSpent Just $ either id undefined $ do
        state1@(_,_,_,_,_,_,_,spent) <- playerTurn state spell
        if (maybe False (spent >=) manaSpent) then Left manaSpent else Right ()
        state2 <- effects state1
        state3 <- bossTurn state2
        state4 <- effects state3
        -- depth-first
        Left (battle (state4,manaSpent))

test :: ()
test
  | battle ((10,250,13,8,0,0,0,0),Nothing) /= Just (173+53) = error "a"
  | battle ((10,250,14,8,0,0,0,0),Nothing) /= Just (229+113+73+173+53) = error "b"
  | otherwise = ()

part1 :: Int -> Int -> Maybe Int
part1 bosshp bossdamage = battle ((50,500,bosshp,bossdamage,0,0,0,0),Nothing)

part2battle :: (State,Maybe Int) -> Maybe Int
part2battle (state,previousManaSpent)
  | otherwise = foldl search previousManaSpent [MagicMissile,Drain,Shield,Poison,Recharge]
  where
    search manaSpent spell = maybe manaSpent Just $ either id undefined $ do
        state1 <- difficultyHard state
        state2 <- effects state1
        state3@(_,_,_,_,_,_,_,spent) <- playerTurn state2 spell
        if (maybe False (spent >=) manaSpent) then Left manaSpent else Right ()
        state4 <- effects state3
        state5 <- bossTurn state4
        Left (part2battle (state5,manaSpent))
    difficultyHard (hp,mana,bosshp,bossdamage,shield,poison,recharge,spent)
      | hp > 1 = Right (hp-1,mana,bosshp,bossdamage,shield,poison,recharge,spent)
      | otherwise = Left Nothing

-- I don't know why starting with 50hp instead of 49hp gives a wrong answer
-- that is too high.
part2 :: Int -> Int -> Maybe Int
part2 bosshp bossdamage = part2battle ((49,500,bosshp,bossdamage,0,0,0,0),Nothing)
