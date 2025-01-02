module AOC202320 where

import Data.Map(Map,alter,empty,fold,fromList,insert,member,size,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2023/input/20",
    aocTests=[
        AOCTest {
            testData=unlines [
                "broadcaster -> a, b, c",
                "%a -> b",
                "%b -> c",
                "%c -> inv",
                "&inv -> a"
                ],
            testResult=Just "32000000",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "broadcaster -> a",
                "%a -> inv, con",
                "&inv -> b",
                "%b -> con",
                "&con -> output"
                ],
            testResult=Just "11687500",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

type State = (Map String Bool,Map String (Map String Bool))
type Module = State -> (String,Bool) -> (State,[((String,Bool),String)])

parse :: String -> (Map String Module,State)
parse = collect . map parseModule . map (words . filter (/= ',')) . lines
  where
    parseModule :: [String] -> (String,([String],Module))
    parseModule (name@"broadcaster":"->":outputs) =
        (name,(outputs,broadcast name outputs))
    parseModule (('%':name):"->":outputs) =
        (name,(outputs,flipflop name outputs))
    parseModule (('&':name):"->":outputs) =
        (name,(outputs,conjunction name outputs))

    collect modules =
        (fromList (map (fmap snd) modules),
         (fromList (map (fmap (const False)) modules),
          Data.Map.map fromList $ foldr collectInputs empty modules))
    collectInputs (src,(dests,_)) inputs =
        foldr (alter (Just . maybe [(src,False)] ((src,False):))) inputs dests

broadcast :: String -> [String] -> Module
broadcast name outputs state (_,pulse) =
    (state,map ((,) (name,pulse)) outputs)

flipflop :: String -> [String] -> Module
flipflop name outputs state (_,True) = (state,[])
flipflop name outputs (self,inputs) (_,False) =
    ((insert name onoff self,inputs),map ((,) (name,onoff)) outputs)
  where onoff = not $ self!name

conjunction :: String -> [String] -> Module
conjunction name outputs (self,inputs) (from,pulse) =
    ((self,insert name input inputs),map ((,) (name,not $ and input)) outputs)
  where input = insert from pulse (inputs!name)

press :: Map String Module -> ((Int,Int),State) -> ((Int,Int),State)
press modules (initialCounts,initialState) =
    dequeue (initialCounts,initialState) [(("button",False),"broadcaster")]
  where
    dequeue :: ((Int,Int),State) -> [((String,Bool),String)]
            -> ((Int,Int),State)
    dequeue (counts,state) [] = (counts,state)
    dequeue ((nlo,nhi),state) (((from,pulse),to):queue) =
        dequeue ((nextlo,nexthi),nextstate) (queue ++ nextqueue)
      where
        nextlo = nlo + if pulse then 0 else 1
        nexthi = nhi + if pulse then 1 else 0
        (nextstate,nextqueue)
          | not (member to modules) = (state,[])
          | otherwise = (modules!to) state (from,pulse)

result (modules,state) =
    uncurry (*) $ fst $ head $ drop 1000 $ iterate (press modules) ((0,0),state)

-- In my input: 
-- rx <- &gf <- &kr, &zs, &kf, &qk

-- broadcaster -> kg dz ff bq
-- the subgraphs from broadcaster to kr zs kf qk are disjoint
-- kg -> kf -- 3767
-- dz -> kr -- 3761
-- ff -> zs -- 4091
-- bq -> qk -- 4001
-- the LCM is 231897990075517

parse2 :: String -> ((Map String Module,State),String)
parse2 input = (parse input,toRx)
  where
    toRx = head $ concatMap (find . words . filter (/= ',')) $ lines input
      where
        find ['&':toRx,"->","rx"] = [toRx]
        find _ = []

press2 :: Map String Module -> String
       -> (Map String Int,(Int,State)) -> (Map String Int,(Int,State))
press2 modules toRx (hiToRx,(pressCount,state)) =
    dequeue (hiToRx,(pressCount+1,state)) [(("button",False),"broadcaster")]
  where
    dequeue :: (Map String Int,(Int,State)) -> [((String,Bool),String)]
            -> (Map String Int,(Int,State))
    dequeue (hiToRx,(pressCount,state)) [] =  (hiToRx,(pressCount,state))
    dequeue (hiToRx,(pressCount,state)) (((from,pulse),to):queue)
      | pulse && to == toRx =
          dequeue (insert from pressCount hiToRx,(pressCount,nextState))
                  (queue ++ nextQueue)
      | otherwise =
          dequeue (hiToRx,(pressCount,nextState)) (queue ++ nextQueue)
      where
        (nextState,nextQueue)
          | not (member to modules) = (state,[])
          | otherwise = (modules!to) state (from,pulse)

getCycles :: ((Map String Module,State),String) -> Map String Int
getCycles ((modules,state@(_,inputs)),rx) =
    fst $ head $ dropWhile toRxPending
        $ iterate (press2 modules rx) (empty,(0,state))
  where
    sizeToRx = size (inputs!rx)
    toRxPending = (sizeToRx >) . size . fst

result2 = fold lcm 1 . getCycles
