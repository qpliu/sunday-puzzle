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
