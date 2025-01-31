module AOC201815 where

import Data.Map(Map,elems,empty,fromList,insert,member,singleton,toList,(!))

import AOC

aoc = AOC {
    day="../../2018/input/15",
    aocTests=[
        AOCTest {
            testData=unlines [
                "#######",
                "#.G...#",
                "#...EG#",
                "#.#.#G#",
                "#..G#E#",
                "#.....#",
                "#######"
                ],
            testResult=Just "27730",
            testResult2=Just "4988"
            },
        AOCTest {
            testData=unlines [
                "#######",
                "#G..#E#",
                "#E#E.E#",
                "#G.##.#",
                "#...#E#",
                "#...E.#",
                "#######"
                ],
            testResult=Just "36334",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "#######",
                "#E..EG#",
                "#.#G.E#",
                "#E.##E#",
                "#G..#.#",
                "#..E#.#",
                "#######"
                ],
            testResult=Just "39514",
            testResult2=Just "31284"
            },
        AOCTest {
            testData=unlines [
                "#######",
                "#E.G#.#",
                "#.#G..#",
                "#G.#.G#",
                "#G..#.#",
                "#...E.#",
                "#######"
                ],
            testResult=Just "27755",
            testResult2=Just "3478"
            },
        AOCTest {
            testData=unlines [
                "#######",
                "#.E...#",
                "#.#..G#",
                "#.###.#",
                "#E#G#G#",
                "#...#G#",
                "#######"
                ],
            testResult=Just "28944",
            testResult2=Just "6474"
            },
        AOCTest {
            testData=unlines [
                "#########",
                "#G......#",
                "#.E.#...#",
                "#..##..G#",
                "#...##..#",
                "#...#...#",
                "#.G...G.#",
                "#.....G.#",
                "#########"
                ],
            testResult=Just "18740",
            testResult2=Just "1140"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=const result,
        pcodeTest2=result2,
        pcodeResult=const result,
        pcodeResult2=result2
        }
    }

parse = (,) 0 . fromList . map toCombatant . toList . parse2d
  where
    toCombatant ((x,y),ch)
      | ch == '.' || ch == '#' = ((y,x),Left ch)
      | otherwise = ((y,x),Right (ch,200,(y,x)))

type YX = (Int,Int)
type State = (Int,Area)
type Combatant = (Char,Int,YX)
type Area = Map YX (Either Char Combatant)

outcome :: State -> Int
outcome (nrounds,area) = nrounds*sum (map getHP (elems area))
  where
    getHP (Right (_,hp,_)) = hp
    getHP _ = 0

doRound :: (Bool,Int) -> State -> Either State (Maybe State)
doRound combatParams state@(_,area) =
    either advance Right $ foldl (turn combatParams) (Left state) combatants
  where
    combatants = concatMap getCombatant $ toList area
    getCombatant (yx,Right (_,_,cid)) = [(yx,cid)]
    getCombatant _ = []

    advance (nrounds,area) = Left (nrounds+1,area)

turn :: (Bool,Int) -> Either State (Maybe State) -> (YX,YX)
     -> Either State (Maybe State)
turn _ ended@(Right _) _ = ended
turn combatParams (Left state@(_,area)) (yx@(y,x),cid)
  | either (const True) (const False) (area!yx) = Left state -- combatant dead
  | cid /= combatantID = Left state -- combatant dead
  | null targets = Right $ Just state -- game over
  | null targetsInRange = move combatParams state yx combatant targets
  | otherwise = attack combatParams state yx combatantTeam targetsInRange
  where
    Right combatant@(combatantTeam,combatantHP,combatantID) = area!yx
    targets = filter isTarget $ toList area
    targetsInRange = filter isInRange targets

    isTarget (_,Right (team,_,_)) = team /= combatantTeam
    isTarget _ = False

    isInRange (targetYX,_) = elem targetYX (yxNextTo yx)

yxNextTo :: YX -> [YX]
yxNextTo (y,x) = [(y-1,x),(y,x-1),(y,x+1),(y+1,x)] -- order is important

move :: (Bool,Int)
     -> State -> YX -> Combatant -> [(YX,Either Char Combatant)]
     -> Either State (Maybe State)
move combatParams
     state@(nrounds,area) yx@(y,x) combatant@(combatantTeam,_,cid) targets
  | null moves = Left state
  | null targetsInRange = Left movedState
  | otherwise = attack combatParams
                       movedState newYX combatantTeam targetsInRange
  where
    movedState =
        (nrounds,insert yx (Left '.') $ insert newYX (Right combatant) area)

    moves :: [YX]
    moves = search (singleton yx yx) (map (:[]) (yxNextTo yx))
    newYX@(newY,newX) = head moves

    search :: Map YX YX -> [[YX]] -> [YX]
    search seen [] = []
    search seen (sYXs@(sYX:_):queue)
      | member sYX seen = search seen queue
      | Left '.' /= area!sYX = search seen queue
      | member sYX nextToTarget = [last sYXs]
      | otherwise =
          search (insert sYX sYX seen) (queue ++ map (:sYXs) (yxNextTo sYX))

    nextToTarget = fromList $ map f $ concatMap (yxNextTo . fst) targets
      where f x = (x,x)

    targetsInRange = filter isInRange targets
    isInRange (targetYX,_) =
        elem targetYX [(newY+1,newX),(newY-1,newX),(newY,newX+1),(newY,newX-1)]

attack :: (Bool,Int)
       -> State -> YX -> Char -> [(YX,Either Char Combatant)]
       -> Either State (Maybe State)
attack (earlyOut,elfPower)
       state@(nrounds,area) yx@(y,x) combatantTeam targets
  | targetHP > attackPower =
      Left (nrounds,
            insert targetYX
                   (Right (targetTeam,targetHP - attackPower,targetID))
                   area)
  | earlyOut && targetTeam == 'E' = Right Nothing
  | otherwise = Left (nrounds,insert targetYX (Left '.') area)
  where
    attackPower | combatantTeam == 'E' = elfPower | otherwise = 3

    (_,(targetYX,Right (targetTeam,targetHP,targetID))) =
        minimum $ map targetSelection targets
    targetSelection target@(_,Right (_,targetHP,_)) = (targetHP,target)

combat :: State -> Int
combat = either combat (maybe (error "") outcome) . doRound (False,3)

result = combat

combat2 :: Int -> State -> Maybe Int
combat2 elfPower =
    either (combat2 elfPower) (fmap outcome) . doRound (True,elfPower)

-- binary search fails because there are higher elf powers that result
-- in elf death
searchMinPower :: Int -> State -> Int
searchMinPower ncpu state = search 3
  where
    search p
      | null runs = search (p+ncpu)
      | otherwise = firstOutcome
      where
        doRun power = combat2 power state
        runs = filter (/= Nothing) $ parallelMap ncpu doRun [p..p+ncpu-1]
        (Just firstOutcome:_) = runs

result2 = searchMinPower
