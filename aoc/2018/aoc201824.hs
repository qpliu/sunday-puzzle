{-
--- Day 24: Immune System Simulator 20XX ---

After a weird buzzing noise, you appear back at the man's cottage. He seems
relieved to see his friend, but quickly notices that the little reindeer caught
some kind of cold while out exploring.

The portly man explains that this reindeer's immune system isn't similar to
regular reindeer immune systems:

The immune system and the infection each have an army made up of several
groups; each group consists of one or more identical units. The armies
repeatedly fight until only one army has units remaining.

Units within a group all have the same hit points (amount of damage a unit can
take before it is destroyed), attack damage (the amount of damage each unit
deals), an attack type, an initiative (higher initiative units attack first and
win ties), and sometimes weaknesses or immunities. Here is an example group:

| 18 units each with 729 hit points (weak to fire; immune to cold, slashing)
|  with an attack that does 8 radiation damage at initiative 10

Each group also has an effective power: the number of units in that group
multiplied by their attack damage. The above group has an effective power of
18 * 8 = 144. Groups never have zero or negative units; instead, the group is
removed from combat.

Each fight consists of two phases: target selection and attacking.

During the target selection phase, each group attempts to choose one target. In
decreasing order of effective power, groups choose their targets; in a tie, the
group with the higher initiative chooses first. The attacking group chooses to
target the group in the enemy army to which it would deal the most damage
(after accounting for weaknesses and immunities, but not accounting for whether
the defending group has enough units to actually receive all of that damage).

If an attacking group is considering two defending groups to which it would
deal equal damage, it chooses to target the defending group with the largest
effective power; if there is still a tie, it chooses the defending group with
the highest initiative. If it cannot deal any defending groups damage, it does
not choose a target. Defending groups can only be chosen as a target by one
attacking group.

At the end of the target selection phase, each group has selected zero or one
groups to attack, and each group is being attacked by zero or one groups.

During the attacking phase, each group deals damage to the target it selected,
if any. Groups attack in decreasing order of initiative, regardless of whether
they are part of the infection or the immune system. (If a group contains no
units, it cannot attack.)

The damage an attacking group deals to a defending group depends on the
attacking group's attack type and the defending group's immunities and
weaknesses. By default, an attacking group would deal damage equal to its
effective power to the defending group. However, if the defending group is
immune to the attacking group's attack type, the defending group instead takes
no damage; if the defending group is weak to the attacking group's attack type,
the defending group instead takes double damage.

The defending group only loses whole units from damage; damage is always dealt
in such a way that it kills the most units possible, and any remaining damage
to a unit that does not immediately kill it is ignored. For example, if a
defending group contains 10 units with 10 hit points each and receives 75
damage, it loses exactly 7 units and is left with 3 units at full health.

After the fight is over, if both armies still contain units, a new fight
begins; combat only ends once one army has lost all of its units.

For example, consider the following armies:

| Immune System:
| 17 units each with 5390 hit points (weak to radiation, bludgeoning) with
|  an attack that does 4507 fire damage at initiative 2
| 989 units each with 1274 hit points (immune to fire; weak to bludgeoning,
|  slashing) with an attack that does 25 slashing damage at initiative 3

| Infection:
| 801 units each with 4706 hit points (weak to radiation) with an attack
|  that does 116 bludgeoning damage at initiative 1
| 4485 units each with 2961 hit points (immune to radiation; weak to fire,
|  cold) with an attack that does 12 slashing damage at initiative 4

If these armies were to enter combat, the following fights, including details
during the target selection and attacking phases, would take place:

| Immune System:
| Group 1 contains 17 units
| Group 2 contains 989 units
| Infection:
| Group 1 contains 801 units
| Group 2 contains 4485 units
| 
| Infection group 1 would deal defending group 1 185832 damage
| Infection group 1 would deal defending group 2 185832 damage
| Infection group 2 would deal defending group 2 107640 damage
| Immune System group 1 would deal defending group 1 76619 damage
| Immune System group 1 would deal defending group 2 153238 damage
| Immune System group 2 would deal defending group 1 24725 damage
| 
| Infection group 2 attacks defending group 2, killing 84 units
| Immune System group 2 attacks defending group 1, killing 4 units
| Immune System group 1 attacks defending group 2, killing 51 units
| Infection group 1 attacks defending group 1, killing 17 units

| Immune System:
| Group 2 contains 905 units
| Infection:
| Group 1 contains 797 units
| Group 2 contains 4434 units
| 
| Infection group 1 would deal defending group 2 184904 damage
| Immune System group 2 would deal defending group 1 22625 damage
| Immune System group 2 would deal defending group 2 22625 damage
| 
| Immune System group 2 attacks defending group 1, killing 4 units
| Infection group 1 attacks defending group 2, killing 144 units

| Immune System:
| Group 2 contains 761 units
| Infection:
| Group 1 contains 793 units
| Group 2 contains 4434 units
| 
| Infection group 1 would deal defending group 2 183976 damage
| Immune System group 2 would deal defending group 1 19025 damage
| Immune System group 2 would deal defending group 2 19025 damage
| 
| Immune System group 2 attacks defending group 1, killing 4 units
| Infection group 1 attacks defending group 2, killing 143 units

| Immune System:
| Group 2 contains 618 units
| Infection:
| Group 1 contains 789 units
| Group 2 contains 4434 units
| 
| Infection group 1 would deal defending group 2 183048 damage
| Immune System group 2 would deal defending group 1 15450 damage
| Immune System group 2 would deal defending group 2 15450 damage
| 
| Immune System group 2 attacks defending group 1, killing 3 units
| Infection group 1 attacks defending group 2, killing 143 units

| Immune System:
| Group 2 contains 475 units
| Infection:
| Group 1 contains 786 units
| Group 2 contains 4434 units
| 
| Infection group 1 would deal defending group 2 182352 damage
| Immune System group 2 would deal defending group 1 11875 damage
| Immune System group 2 would deal defending group 2 11875 damage
| 
| Immune System group 2 attacks defending group 1, killing 2 units
| Infection group 1 attacks defending group 2, killing 142 units

| Immune System:
| Group 2 contains 333 units
| Infection:
| Group 1 contains 784 units
| Group 2 contains 4434 units
| 
| Infection group 1 would deal defending group 2 181888 damage
| Immune System group 2 would deal defending group 1 8325 damage
| Immune System group 2 would deal defending group 2 8325 damage
| 
| Immune System group 2 attacks defending group 1, killing 1 unit
| Infection group 1 attacks defending group 2, killing 142 units

| Immune System:
| Group 2 contains 191 units
| Infection:
| Group 1 contains 783 units
| Group 2 contains 4434 units
| 
| Infection group 1 would deal defending group 2 181656 damage
| Immune System group 2 would deal defending group 1 4775 damage
| Immune System group 2 would deal defending group 2 4775 damage
| 
| Immune System group 2 attacks defending group 1, killing 1 unit
| Infection group 1 attacks defending group 2, killing 142 units

| Immune System:
| Group 2 contains 49 units
| Infection:
| Group 1 contains 782 units
| Group 2 contains 4434 units
| 
| Infection group 1 would deal defending group 2 181424 damage
| Immune System group 2 would deal defending group 1 1225 damage
| Immune System group 2 would deal defending group 2 1225 damage
| 
| Immune System group 2 attacks defending group 1, killing 0 units
| Infection group 1 attacks defending group 2, killing 49 units

| Immune System:
| No groups remain.
| Infection:
| Group 1 contains 782 units
| Group 2 contains 4434 units

In the example above, the winning army ends up with 782 + 4434 = 5216 units.

You scan the reindeer's condition (your puzzle input); the white-bearded man
looks nervous. As it stands now, how many units would the winning army have?
-}

import Control.Monad.State(State,get,put,runState)
import Data.List(sort)

data Group = Group {
    gId :: String,
    gUnits :: Int,
    gHP :: Int,
    gWeak :: [String],
    gImmune :: [String],
    gDamage :: Int,
    gAttack :: String,
    gInitiative :: Int
    }
  deriving (Eq,Ord,Show)
    
type Combat = ([Group],[Group])

parse :: String -> Combat
parse = fmap (fst . parseArmy) . parseArmy . words
  where
    parseArmy ("Immune":"System:":rest) = parseGroups "im" [] rest
    parseArmy ("Infection:":rest) = parseGroups "inf" [] rest
    parseGroups army groups (units:"units":"each":"with":hp:"hit":"points":rest) = parseImmune army groups Group{gId=army++show (1+length groups),gUnits=read units,gHP=read hp,gWeak=[],gImmune=[],gDamage=undefined,gAttack=undefined,gInitiative=undefined} rest
    parseGroups army groups rest = (groups,rest)
    parseImmune army groups group ("(immune":"to":rest) = parseImmuneList army groups group rest
    parseImmune army groups group ("immune":"to":rest) = parseImmuneList army groups group rest
    parseImmune army groups group rest = parseWeak army groups group rest
    parseImmuneList army groups group (attack:rest)
      | last attack == ';' = parseWeak army groups group{gImmune=reverse (tail (reverse attack)):gImmune group} rest
      | last attack == ',' = parseImmuneList army groups group{gImmune=reverse (tail (reverse attack)):gImmune group} rest
      | last attack == ')' = parseAttack army groups group{gImmune=reverse (tail (reverse attack)):gImmune group} rest
    parseWeak army groups group ("(weak":"to":rest) = parseWeakList army groups group rest
    parseWeak army groups group ("weak":"to":rest) = parseWeakList army groups group rest
    parseWeak army groups group rest = parseAttack army groups group rest
    parseWeakList army groups group (attack:rest)
      | last attack == ';' = parseImmune army groups group{gWeak=reverse (tail (reverse attack)):gWeak group} rest
      | last attack == ',' = parseWeakList army groups group{gWeak=reverse (tail (reverse attack)):gWeak group} rest
      | last attack == ')' = parseAttack army groups group{gWeak=reverse (tail (reverse attack)):gWeak group} rest
    parseAttack army groups group ("with":"an":"attack":"that":"does":damage:attack:"damage":"at":"initiative":initiative:rest) = parseGroups army (group{gDamage=read damage,gAttack=attack,gInitiative=read initiative}:groups) rest

getGroup :: String -> State Combat [Group]
getGroup groupId = do
    (army1,army2) <- get
    return $ filter matches (army1 ++ army2)
  where
    matches Group{gId=gId2} = gId2 == groupId

updateGroup :: Group -> State Combat ()
updateGroup group1@Group{gId=gId1} = do
    (army1,army2) <- get
    put (map replace army1,map replace army2)
  where
    replace group2@Group{gId=gId2} | gId1 == gId2 = group1 | otherwise = group2

removeGroup :: String -> State Combat ()
removeGroup groupId = do
    (army1,army2) <- get
    put (filter notMatches army1,filter notMatches army2)
  where
    notMatches Group{gId=gId2} = gId2 /= groupId

getEffectivePower :: Group -> (Int,Int)
getEffectivePower Group{gUnits=u,gDamage=d,gInitiative=i} = (u*d,i)

getDamage :: Group -> Group -> (Int,(Int,Int))
getDamage Group{gUnits=units,gAttack=attack,gDamage=damage} defender@Group{gImmune=immune,gWeak=weak}
  | attack `elem` immune = (0,getEffectivePower defender)
  | attack `elem` weak = (2*units*damage,getEffectivePower defender)
  | otherwise = (units*damage,getEffectivePower defender)

chooseTargets :: Combat -> [(Int,String,String)]
chooseTargets (army1,army2) = reverse $ sort $ choose a1 a2 ++ choose a2 a1
  where
    a1 = inTargetSelectionOrder army1
    a2 = inTargetSelectionOrder army2
    inTargetSelectionOrder groups =
        reverse $ map snd $ sort $ zip (map effectivePower groups) groups
    effectivePower Group{gUnits=u,gDamage=d,gInitiative=i} = (u*d,i)

    choose [] _ = []
    choose (attacker:attackers) targets = selection ++ choose attackers rest
      where (selection,rest) = selectTarget attacker targets
    selectTarget attacker@Group{gId=aId,gInitiative=aInitiative} defenders
      | null (filter ((> 0) . fst . fst) targets) = ([],defenders)
      | otherwise = ([(aInitiative,aId,def1)],defs)
      where
        targets = zip (map (getDamage attacker) defenders) defenders
        (Group{gId=def1}:defs) = reverse $ map snd $ sort targets

fight :: State Combat Bool
fight = do
    attacks <- fmap chooseTargets get
    mapM_ doAttack attacks
    (army1,army2) <- get
    return $ null army1 || null army2

doAttack :: (Int,String,String) -> State Combat ()
doAttack (_,attackerId,defenderId) = do
    [defender@Group{gUnits=units,gHP=hp}] <- getGroup defenderId
    attackers <- getGroup attackerId
    if null attackers
      then
          return ()
      else do
          let (damage,_) = getDamage (head attackers) defender
          let unitsKilled = damage `div` hp
          if unitsKilled < units
            then updateGroup defender{gUnits=units-unitsKilled}
            else removeGroup defenderId

combat :: Combat -> Combat
combat state | done = nextState | otherwise = combat nextState
  where (done,nextState) = runState fight state

totalUnits :: Combat -> Int
totalUnits (army1,army2) = sum (map gUnits army1 ++ map gUnits army2)

testData :: String
testData = "Immune System:\n17 units each with 5390 hit points (weak to radiation, bludgeoning) with\n an attack that does 4507 fire damage at initiative 2\n989 units each with 1274 hit points (immune to fire; weak to bludgeoning,\n slashing) with an attack that does 25 slashing damage at initiative 3\n\nInfection:\n801 units each with 4706 hit points (weak to radiation) with an attack\n that does 116 bludgeoning damage at initiative 1\n4485 units each with 2961 hit points (immune to radiation; weak to fire,\n cold) with an attack that does 12 slashing damage at initiative 4"

test :: ()
test
  | totalUnits (combat $ parse testData) /= 5216 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (totalUnits . combat . parse) $ readFile "input/24.txt"

addBoost :: Int -> Combat -> Combat
addBoost boost (im,inf) = (map addB im,inf)
  where addB g@Group{gDamage=damage} = g{gDamage=damage+boost}

test2 :: ()
test2
  | totalUnits (combat $ addBoost 1570 $ parse testData) /= 51 = error "a"
  | otherwise = ()

-- With my input data, boosts of 31 and 32 result in stalemate,
-- with both sides having one group that cannot damage the other.
part2 :: IO Int
part2 = fmap (totalUnits . combat . addBoost 33 . parse) $ readFile "input/24.txt"
