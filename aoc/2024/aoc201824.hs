module AOC201824 where

import Data.List(partition,sort)
import Data.Map(Map,adjust,delete,empty,insert,mapKeys,toList,(!))
import qualified Data.Map
import Data.Set(Set)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2018/input/24",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Immune System:",
                "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2",
                "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3",
                "",
                "Infection:",
                "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1",
                "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"
                ],
            testResult=Just "5216",
            testResult2=Just "51"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

type UnitID = (Int,Unit)
data Unit = Unit Int Int String Int (Set String) (Set String)
    deriving (Eq,Ord,Show)

parse = Data.Map.fromList . concat . zipWith parseArmy [0..]
                          . parseBlankLineSeparated
  where
    parseArmy side = map (parseUnit side) . tail
    parseUnit side text = ((side,unit),count)
      where
        unit = Unit hp attack attackType initiative
                    (Data.Set.fromList immune) (Data.Set.fromList weak)
        [count,hp,attack,initiative] = parseInts text
        (_:_:attackType:_) = dropWhile (/= "does") $ words text
        weak = getTypes "weak"
        immune = getTypes "immune"
        getTypes t = extractTypes $ drop 2 $ dropWhile notIntro $ words text
          where notIntro w = w /= t && drop 1 w /= t
        extractTypes [] = []
        extractTypes (w:ws)
          | last w == ',' = init w : extractTypes ws
          | otherwise = [init w]

combat :: Map UnitID Int -> (Int,Int)
combat groups
  | null side0 = (1,sum $ map snd $ side1)
  | null side1 = (0,sum $ map snd $ side0)
  | groups == nextGroups = (-1,0) -- stalemate
  | otherwise = combat nextGroups
  where
    (side0,side1) = partition ((== 0) . fst . fst) $ toList groups
    targets = targetSelection (sort $ map addSelectionMetric side1) side0
           ++ targetSelection (sort $ map addSelectionMetric side0) side1
    addSelectionMetric group@(unitID@(_,Unit _ attack attackType
                                             initiative _ _),count) =
        ((-count*attack,-initiative),group)
    nextGroups = attack groups $ sort targets

targetSelection :: [(a,(UnitID,Int))] -> [(UnitID,Int)]
                -> [(Int,UnitID,UnitID)]
targetSelection [] _ = []
targetSelection _ [] = []
targetSelection ((_,(attackerID@(_,Unit _ attack attackType initiative _ _),_))
                 :attackers)
                defenders
  | null prioritizedDefenders || selectedDamage == 0 =
      targetSelection attackers defenders
  | otherwise = (initiative,attackerID,selectedDefenderID) :
                    targetSelection attackers (map snd remainingDefenders)
  where
    prioritizedDefenders =
        sort [(priority defender,defender) | defender <- defenders]
    (((selectedDamage,_,_),(selectedDefenderID,_)):remainingDefenders) =
        prioritizedDefenders

    priority ((_,Unit hp defAttack _ defInitiative immune weak),defCount)
      | Data.Set.member attackType immune =
          (0,-defAttack*defCount,-defInitiative)
      | Data.Set.member attackType weak =
          (-2*attack,-defAttack*defCount,-defInitiative)
      | otherwise =
          (-attack,-defAttack*defCount,-defInitiative)

attack :: Map UnitID Int -> [(a,UnitID,UnitID)] -> Map UnitID Int
attack groups targets = foldr doAttack groups targets
  where
    addInitiative matchup@(_,(_,Unit _ _ _ initiative _ _)) =
        (initiative,matchup)
    doAttack (_,attackerID@(attackTeam,Unit _ attack attackType _ _ _),
                defenderID@(_,Unit hp _ _ _ immune weak))
             groups
      | not (Data.Map.member attackerID groups) = groups
      | kills < groups!defenderID = adjust (+ (-kills)) defenderID groups
      | otherwise = delete defenderID groups
      where
        attackPower
          | Data.Set.member attackType immune = error "0"
          | Data.Set.member attackType weak = 2*attack*groups!attackerID
          | otherwise = attack*groups!attackerID
        kills = attackPower `div` hp

result = snd . combat

boost :: Int -> Map UnitID Int -> Map UnitID Int
boost increase = mapKeys addBoost
  where
    addBoost unitID@(team,Unit hp attack attackType initiative immune weak)
      | team == 0 = (team,Unit hp (attack+increase) attackType
                               initiative immune weak)
      | otherwise = unitID

result2 groups = findUpperBound 0 40
  where
    findUpperBound lower upper
      | winner == 0 = binarySearch lower upper winnerUnits
      | otherwise = findUpperBound upper (upper*2)
      where (winner,winnerUnits) = combat $ boost upper groups
    binarySearch lower upper upperUnits
      | lower+1 == upper = upperUnits
      | winner == 0 = binarySearch lower mid winnerUnits
      | otherwise = binarySearch mid upper upperUnits
      where
        (winner,winnerUnits) = combat $ boost mid groups
        mid = (lower+upper) `div` 2
