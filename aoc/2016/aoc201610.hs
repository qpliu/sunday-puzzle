{-
--- Day 10: Balance Bots ---

You come upon a factory in which many robots are zooming around handing small
microchips to each other.

Upon closer examination, you notice that each bot only proceeds when it has two
microchips, and once it does, it gives each one to a different bot or puts it
in a marked "output" bin. Sometimes, bots take microchips from "input" bins,
too.

Inspecting one of the microchips, it seems like they each contain a single
number; the bots must use some logic to decide what to do with each chip. You
access the local control computer and download the bots' instructions (your
puzzle input).

Some of the instructions specify that a specific-valued microchip should be
given to a specific bot; the rest of the instructions indicate what a given bot
should do with its lower-value or higher-value chip.

For example, consider the following instructions:

value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2

 - Initially, bot 1 starts with a value-3 chip, and bot 2 starts with a value-2
   chip and a value-5 chip.
 - Because bot 2 has two microchips, it gives its lower one (2) to bot 1 and
   its higher one (5) to bot 0.
 - Then, bot 1 has two microchips; it puts the value-2 chip in output 1 and
   gives the value-3 chip to bot 0.
 - Finally, bot 0 has two microchips; it puts the 3 in output 2 and the 5 in
   output 0.

In the end, output bin 0 contains a value-5 microchip, output bin 1 contains a
value-2 microchip, and output bin 2 contains a value-3 microchip. In this
configuration, bot number 2 is responsible for comparing value-5 microchips
with value-2 microchips.

Based on your instructions, what is the number of the bot that is responsible
for comparing value-61 microchips with value-17 microchips?
-}

-- I suspect part 2 will involve looking at the output bins.

import Data.Map(Map,adjust,fromList,insert,toList,(!))

type State = (Map Int [Int],Map Int [Int])

type Rules = Map Int (State -> [Int] -> State)

setupState :: [[String]] -> State
setupState insns = (fromList $ concatMap setupBots insns,fromList $ concatMap setupOutputs insns)
  where
    setupBots ("value":_:"goes":"to":"bot":bot:_) = [(read bot,[])]
    setupBots ("bot":bota:"gives":"low":"to":"bot":botb:"and":"high":"to":"bot":botc:_) = [(read bota,[]),(read botb,[]),(read botc,[])]
    setupBots ("bot":bota:"gives":"low":"to":"output":_:"and":"high":"to":"bot":botc:_) = [(read bota,[]),(read botc,[])]
    setupBots ("bot":bota:"gives":"low":"to":"bot":botb:_) = [(read bota,[]),(read botb,[])]
    setupBots ("bot":bot:_) = [(read bot,[])]
    setupBots _ = []
    setupOutputs ("bot":_:"gives":"low":"to":"output":outputa:"and":"high":"to":"output":outputb:_) = [(read outputa,[]),(read outputb,[])]
    setupOutputs ("bot":_:"gives":"low":"to":"bot":_:"and":"high":"to":"output":outputb:_) = [(read outputb,[])]
    setupOutputs ("bot":_:"gives":"low":"to":"output":outputa:_) = [(read outputa,[])]
    setupOutputs _ = []

initState :: [[String]] -> (Rules,State)
initState insns = (fromList $ concatMap initRule insns,(foldl initBot setupBots insns,outputs))
  where
    (setupBots,outputs) = setupState insns
    initBot bots ("value":val:"goes":"to":"bot":bot:_) = adjust (read val:) (read bot) bots
    initBot state _ = state
    initRule ("bot":bota:"gives":"low":"to":"bot":botb:"and":"high":"to":"bot":botc:_) = [(read bota,give (toBot (read botb)) (toBot (read botc)))]
    initRule ("bot":bota:"gives":"low":"to":"output":outputb:"and":"high":"to":"bot":botc:_) = [(read bota,give (toOutput (read outputb)) (toBot (read botc)))]
    initRule ("bot":bota:"gives":"low":"to":"bot":botb:"and":"high":"to":"output":outputc:_) = [(read bota,give (toBot (read botb)) (toOutput (read outputc)))]
    initRule ("bot":bota:"gives":"low":"to":"output":outputb:"and":"high":"to":"output":outputc:_) = [(read bota,give (toOutput (read outputb)) (toOutput (read outputc)))]
    initRule _ = []
    give givelow givehigh state chips = givelow (minimum chips) $ givehigh (maximum chips) state
    toBot bot chip (bots,outputs) = (adjust (chip:) bot bots,outputs)
    toOutput output chip (bots,outputs) = (bots,adjust (chip:) output outputs)

step :: Rules -> State -> Maybe State
step rules (bots,outputs) = doBot (toList bots)
  where
    doBot [] = Nothing
    doBot ((bot,chips):rest)
      | length chips < 2 = doBot rest
      | otherwise = Just $ (rules!bot) (insert bot [] bots,outputs) chips

check :: State -> Maybe Int
check (bots,outputs)
  | null matching = Nothing
  | otherwise = Just (fst (head matching))
  where
    matching = filter matches (toList bots)
    matches (_,chips) = length chips >= 2 && minimum chips == 17 && maximum chips == 61

run :: (State -> Maybe a) -> String -> Either a State
run check insns = doStep state0
  where
    (rules,state0) = initState (map words $ lines insns)
    doStep state = maybe (maybe (Right state) doStep (step rules state)) Left (check state)

test :: ()
test
  | state0 /= (fromList [(0,[]),(1,[3]),(2,[2,5])],fromList [(0,[]),(1,[]),(2,[])]) = error "a"
  | state1 /= (fromList [(0,[5]),(1,[2,3]),(2,[])],fromList [(0,[]),(1,[]),(2,[])]) = error "b"
  | state2 /= (fromList [(0,[3,5]),(1,[]),(2,[])],fromList [(0,[]),(1,[2]),(2,[])]) = error "c"
  | state3 /= (fromList [(0,[]),(1,[]),(2,[])],fromList [(0,[5]),(1,[2]),(2,[3])]) = error "d"
  | Nothing /= step rules state3 = error "e"
  | otherwise = ()
  where
    (rules,state0) = initState $ map words $ lines "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2"
    Just state1 = step rules state0
    Just state2 = step rules state1
    Just state3 = step rules state2

part1 :: IO (Either Int State)
part1 = fmap (run check) $ readFile "input/10.txt"

run2 :: String -> Int
run2 insns = head (outputs!0)*head (outputs!1)*head (outputs!2)
  where
    Right (bots,outputs) = run (const Nothing) insns

part2 = fmap run2 $ readFile "input/10.txt"
