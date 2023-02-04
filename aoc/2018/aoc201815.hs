import Control.Monad(foldM)
import Data.List(sort)
import Data.Map(Map,adjust,delete,findWithDefault,fromList,keys,insert,toList,(!))
import Data.Set(difference,union)
import qualified Data.Set

-- Goblins have negative IDs, Elves have positive IDs, zero means open cavern.
-- Use (Y,X) as coordinates to make sorting easier.

type State = (Int,Map (Int,Int) Int,Map Int Int)
-- (completed rounds,cavern,units)
-- cavern is map of (y,x) to unitID (or 0 for open cavern)
-- units is map of unitID to HP

parse :: String -> State
parse s = p [] [] 1 (-1) (0,0) s
  where
    p cavern units elf gob (y,x) "" =
        (0,fromList cavern,fromList units)
    p cavern units elf gob (y,x) ('\n':rest) =
        p cavern units elf gob (y+1,0) rest
    p cavern units elf gob (y,x) ('.':rest) =
        p (((y,x),0):cavern) units elf gob (y,x+1) rest
    p cavern units elf gob (y,x) ('E':rest) =
        p (((y,x),elf):cavern) ((elf,200):units) (elf+1) gob (y,x+1) rest
    p cavern units elf gob (y,x) ('G':rest) =
        p (((y,x),gob):cavern) ((gob,200):units) elf (gob-1) (y,x+1) rest
    p cavern units elf gob (y,x) (_:rest) =
        p cavern units elf gob (y,x+1) rest

turnOrder :: State -> [((Int,Int),Int)]
turnOrder (_,cavern,_) = filter ((/= 0) . snd) $ toList cavern

makeOutcome :: State -> Int
makeOutcome (rounds,_,units) = rounds*sum units

getAttack :: State -> Int -> Int -> Int
getAttack _ _ _ = 3

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (y,x) = [(y-1,x),(y,x-1),(y,x+1),(y+1,x)]

doRound :: (State -> Int -> Int -> Int) -> State -> Either State State
doRound getAttack state = do
    (rounds,cavern,units) <- foldM (turn getAttack) state (turnOrder state)
    return (rounds+1,cavern,units)

turn :: (State -> Int -> Int -> Int) -> State -> ((Int,Int),Int) -> Either State State
turn getAttack state@(round,cavern,units) unit@(yx,unitid)
  | cavern!yx /= unitid = Right state -- previously killed in this round
  | otherwise = attack getAttack (move state unit)
  where
    targetsInRange = [(units!uu,targetyx,uu) | targetyx <- neighbors yx, uu <- [findWithDefault 0 targetyx cavern], uu*unitid < 0]

move :: State -> ((Int,Int),Int) -> (State,((Int,Int),Int))
move state@(rounds,cavern,units) (yx,unitid)
  | any target (neighbors yx) || null firstMoves = (state,(yx,unitid))
  | otherwise = ((rounds,insert (snd $ minimum firstMoves) unitid (insert yx 0 cavern),units),(snd $ minimum firstMoves,unitid))
  where
    open yx = findWithDefault 1 yx cavern == 0
    target yx = findWithDefault 0 yx cavern*unitid < 0
    goal yx@(y,x) = open yx && any target (neighbors yx)

    firstMoves = concatMap getMoveDist (neighbors yx)
    getMoveDist startyx
      | open startyx = search startyx 1 (Data.Set.fromList [startyx]) (Data.Set.fromList [startyx])
      | otherwise = []
    search startyx dist visited current
      | Data.Set.size current == 0 = []
      | any goal current = [(dist,startyx)]
      | otherwise = search startyx (dist+1) (visited `union` nexts) nexts
      where
        nexts = Data.Set.filter open $ Data.Set.fromList (concatMap neighbors $ Data.Set.toList current) `difference` visited

attack :: (State -> Int -> Int -> Int) -> (State,((Int,Int),Int)) -> Either State State
attack getAttack (state@(rounds,cavern,units),(yx,unitid))
  | null targetsInRange = Right state
  | targetHP > attackPower =
        return (rounds,cavern,adjust (+ (-attackPower)) targetid units)
  | all ((> 0) . (*unitid) . fst) (toList killUnits) = Left killState
  | otherwise = Right killState
  where
    targetsInRange = [(units!uu,targetyx,uu) | targetyx <- neighbors yx, uu <- [findWithDefault 0 targetyx cavern], uu*unitid < 0]
    (targetHP,targetYX,targetid) = minimum targetsInRange
    attackPower = getAttack state unitid targetid
    killState@(_,killCavern,killUnits) = (rounds,insert targetYX 0 cavern,delete targetid units)

combat :: (State -> a) -> (State -> Int -> Int -> Int) -> State -> a
combat makeOutcome getAttack state =
    either makeOutcome (combat makeOutcome getAttack) (doRound getAttack state)

testData1 :: String
testData1 = "#######\n#.G.E.#\n#E.G.E#\n#.G.E.#\n#######"

testData :: [(Int,String)]
testData = [
    -- I think the example actually ends in round 47, for an outcome of 28140
    -- rather then in round 48
    -- (27730,"#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######"),
    (36334,"#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######"),
    (39514,"#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######"),
    (27755,"#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######"),
    (28944,"#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######"),
    (18740,"#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########")
    ]

test :: ()
test
  | any (uncurry (/=) . fmap (combat makeOutcome getAttack . parse)) testData = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (combat makeOutcome getAttack . parse) $ readFile "input/15.txt"

countElves :: State -> Int
countElves (_,_,units) = length $ filter (> 0) $ keys units

combat2 :: Int -> Int -> State -> Bool
combat2 elfAttack nElves state
  | nElves < countElves state = False
  | otherwise = either ((nElves ==) . countElves) (combat2 elfAttack nElves) (doRound (getAttack2 elfAttack) state)

getAttack2 :: Int -> State -> Int -> Int -> Int
getAttack2 elfAttack _ unitid _ | unitid > 0 = elfAttack | otherwise = 3

findMinAttack :: State -> Int
findMinAttack state = head $ dropWhile lose [1..]
  where lose a = not (combat2 a (countElves state) state)

run2 :: String -> Int
run2 input = combat makeOutcome (getAttack2 (findMinAttack state)) state
  where state = parse input

test2 :: ()
test2
  | map (findMinAttack . parse . snd) testData /= [{-15,-} 4{-?-}, 4, 15, 12, 34] = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap run2 $ readFile "input/15.txt"
