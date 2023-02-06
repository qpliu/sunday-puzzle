import Debug.Trace(traceShow)

import Data.Set(Set,fromList,maxView,singleton,size,union)

type Blueprint = (Int,(Int,Int,(Int,Int),(Int,Int)))

parse :: String -> [Blueprint]
parse = p . words
  where
    p ("Blueprint":idNumber:"Each":"ore":"robot":"costs":ore:"ore.":
       "Each":"clay":"robot":"costs":clay:"ore.":
       "Each":"obsidian":"robot":"costs":obsidian1:"ore":"and":obsidian2:"clay.":
       "Each":"geode":"robot":"costs":geode1:"ore":"and":geode2:"obsidian.":rest) =
        (read (filter (/= ':') idNumber),
            (read ore,read clay,
            (read obsidian1,read obsidian2),(read geode1,read geode2))) : p rest
    p _ = []

type State = ((Int,Int),(Int,Int),(Int,Int),(Int,Int),Int)
-- (geode,geodeBots),(obs,obsBots),(clay,clayBots),(ore,oreBots),minutes
-- put geode first to sort by geode first

start :: State
start = ((0,0),(0,0),(0,0),(0,1),24)

doBots :: State -> State
doBots ((geode,geodeBots),(obsidian,obsidianBots),(clay,clayBots),(ore,oreBots),minutes) =
    ((geode+geodeBots,geodeBots),(obsidian+obsidianBots,obsidianBots),(clay+clayBots,clayBots),(ore+oreBots,oreBots),minutes-1)

choices :: Blueprint -> State -> [State]
choices (_,(oreBotOreCost,clayBotOreCost,(obsBotOreCost,obsBotClayCost),(geoBotOreCost,geoBotObsCost))) state@(_,(obsSupply,_),(claySupply,_),(oreSupply,_),_)
  | null options = [makeNothing]
  | otherwise = options
  where
    makeNothing@(geoState@(geo,geoBots),obsState@(obs,obsBots),clayState@(clay,clayBots),oreState@(ore,oreBots),minutes) = doBots state
    options =
      (if oreSupply >= geoBotOreCost && obsSupply >= geoBotObsCost
        then [((geo,geoBots+1),(obs-geoBotObsCost,obsBots),clayState,(ore-geoBotOreCost,oreBots),minutes)]
        else [])
      ++
      (if oreSupply >= obsBotOreCost && claySupply >= obsBotClayCost && obs + obsBots*minutes <= geoBotObsCost*minutes
        then [(geoState,(obs,obsBots+1),(clay-obsBotClayCost,clayBots),(ore-obsBotOreCost,oreBots),minutes)]
        else [])
      ++
      (if oreSupply >= clayBotOreCost && clay + clayBots*minutes <= minutes*obsBotClayCost
        then [(geoState,obsState,(clay,clayBots+1),(ore-clayBotOreCost,oreBots),minutes)]
        else [])
      ++
      (if oreSupply >= oreBotOreCost && ore + oreBots*minutes <= maximum [clayBotOreCost,obsBotOreCost,geoBotOreCost]*minutes
        then [(geoState,obsState,clayState,(ore-oreBotOreCost,oreBots+1),minutes)]
        else [])
      ++
      (if (oreSupply < maximum [geoBotOreCost,obsBotOreCost,clayBotOreCost] && oreBots > 0) || (claySupply < obsBotClayCost && clayBots > 0) || (obsSupply < geoBotObsCost && obsBots > 0)
        then [makeNothing]
        else [])

-- Comprehensive depth-first search is too slow.
search :: Blueprint -> State -> State
search blueprint state@(_,_,_,_,minutes)
  | minutes <= 0 = state
  | otherwise = maximum $ map (search blueprint) $ choices blueprint state

-- https://en.wikipedia.org/wiki/SSS*
-- Still might be too slow, since the potential quadratic growth
-- makes deprioritizing suboptimal paths less effective, especially
-- early on, but at least it's not exponential growth.
searchS :: Blueprint -> State -> State
searchS blueprint start = sss 500 (singleton (h start,start))
  where
    h :: State -> Int
    h ((geo,geoBots),_,_,_,minutes) = geo + minutes*geoBots + minutes*(minutes+1) `div` 2

    sss :: Int -> Set (Int,State) -> State
    sss lastMinutes open
      | minutes /= lastMinutes && traceShow (size open,current) False = undefined
      | minutes <= 0 = current
      | otherwise = sss minutes $ union newOpen $ fromList [(h st,st) | st <- choices blueprint current]
      where
        Just ((_,current@(_,_,_,_,minutes)),newOpen) = maxView open

qualityLevel :: Blueprint -> Int
qualityLevel blueprint@(idNum,_) = idNum*geo
  where ((geo,_),_,_,_,_) = searchS blueprint start

testData :: String
testData = concat [
    "Blueprint 1:",
    "  Each ore robot costs 4 ore.",
    "  Each clay robot costs 2 ore.",
    "  Each obsidian robot costs 3 ore and 14 clay.",
    "  Each geode robot costs 2 ore and 7 obsidian.",
    "\n",
    "Blueprint 2:",
    "  Each ore robot costs 2 ore.",
    "  Each clay robot costs 3 ore.",
    "  Each obsidian robot costs 3 ore and 8 clay.",
    "  Each geode robot costs 3 ore and 12 obsidian.",
    "\n"
    ]

test :: ()
test
  | (map qualityLevel . parse) testData /= [9,24] = error "a"
  | otherwise = ()

-- This is still very slow.  And part 2 is even slower.
part1 :: IO Int
part1 = fmap (sum . map qualityLevel . parse) $ readFile "input/19.txt"

start2 :: State
start2 = ((0,0),(0,0),(0,0),(0,1),32)

bestGeo :: Blueprint -> Int
bestGeo blueprint = geo
  where ((geo,_),_,_,_,_) = searchS blueprint start2

-- Gets the wrong answer for blueprint 1, but the right answer for blueprint 2
-- and the input data.
test2 :: ()
test2
  | (map bestGeo . parse) testData /= [56,62] = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (product . map bestGeo . take 3 . parse) $ readFile "input/19.txt"
