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
