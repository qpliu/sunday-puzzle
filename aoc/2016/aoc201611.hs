-- A lower bound would be to always bring two items up and one item down,
-- ignoring the constraints.  In the example, one step brings everything on
-- floor 1 to floor 2, three more steps to bring everything on floor 2 to
-- floor 3, then five more steps to bring everything to floor 4 for a total
-- of nine steps.

-- I think the constraints will limit the number of possible choices severely
-- enough to make a breadth-first search possible.  I don't think it will ever
-- be necessary to bring two items down, so the choices are
--  bring up a chip-generator pair
--  bring up a chip
--  bring up 2 chips
--  bring up a generator
--  bring up 2 generators
--  bring down a chip
--  bring down a generator

import Data.Map(Map,adjust,insert,(!))
import qualified Data.Map
import Data.Set(Set,empty,fromList,member,union)
import qualified Data.Set

type Floor = (Map String Int,Map String Int) -- (number of chips by type,number of generators by type)

type State = ([Floor],[Floor]) -- (current floor and floors above,floors below)

parse :: String -> State
parse input = ([parseFloor "first",parseFloor "second",parseFloor "third",parseFloor "fourth"],[])
  where
    parseFloor name = scan (nothing,nothing) $ concat $ filter ((== [name]) . take 1 . drop 1) $ map words $ lines input
      where
        scan (chips,gens) [] = (chips,gens)
        scan (chips,gens) (t:"generator":rest) = scan (chips,adjust (+1) t gens) rest
        scan (chips,gens) (t:"generator,":rest) = scan (chips,adjust (+1) t gens) rest
        scan (chips,gens) (t:"generator.":rest) = scan (chips,adjust (+1) t gens) rest
        scan (chips,gens) (n:t:"generators":rest) = scan (chips,adjust (+read n) t gens) rest
        scan (chips,gens) (n:t:"generators,":rest) = scan (chips,adjust (+read n) t gens) rest
        scan (chips,gens) (n:t:"generators.":rest) = scan (chips,adjust (+read n) t gens) rest
        scan (chips,gens) (t:"microchip":rest) = scan (adjust (+1) (takeWhile (/= '-') t) chips,gens) rest
        scan (chips,gens) (t:"microchip,":rest) = scan (adjust (+1) (takeWhile (/= '-') t) chips,gens) rest
        scan (chips,gens) (t:"microchip.":rest) = scan (adjust (+1) (takeWhile (/= '-') t) chips,gens) rest
        scan (chips,gens) (n:t:"microchips":rest) = scan (adjust (+read n) (takeWhile (/= '-') t) chips,gens) rest
        scan (chips,gens) (n:t:"microchips,":rest) = scan (adjust (+read n) (takeWhile (/= '-') t) chips,gens) rest
        scan (chips,gens) (n:t:"microchips.":rest) = scan (adjust (+read n) (takeWhile (/= '-') t) chips,gens) rest
        scan (chips,gens) (_:rest) = scan (chips,gens) rest
        
    nothing = Data.Map.fromList $ map (flip (,) 0) $ Data.Set.toList types
    types = fromList $ scan $ words input
      where
        scan [] = []
        scan (t:"generator":rest) = t : scan rest
        scan (t:"generator.":rest) = t : scan rest
        scan (t:"generator,":rest) = t : scan rest
        scan (_:rest) = scan rest

ok :: Floor -> Bool
ok (chips,generators)
  | sum generators == 0 = True
  | otherwise = and [n <= generators!t | (t,n) <- Data.Map.toList chips]

done :: State -> Bool
done ([_],down) = all clear down
  where clear (chips,gens) = sum chips == 0 && sum gens == 0
done _ = False

step :: Set State -> State -> [State]
step _ ([],_) = error "impossible"
step seenStates (floor:up,down) =
  filter (not . (`member` seenStates)) $
      (if null down then [] else
          concatMap (put1chip (head down) (\ (startFloor,destFloor) -> destFloor:startFloor:up) (const (tail down))) (take1chip floor) ++
          concatMap (put1gen (head down) (\ (startFloor,destFloor) -> destFloor:startFloor:up) (const (tail down))) (take1gen floor)) ++
      (if null up then [] else
          concatMap (put1chip (head up) (\ (_,destFloor) -> destFloor:tail up) (\ (startFloor,_) -> startFloor:down)) (take1chip floor) ++
          concatMap (put1gen (head up) (\ (_,destFloor) -> destFloor:tail up) (\ (startFloor,_) -> startFloor:down)) (take1gen floor) ++
          concatMap (put1pair (head up) (\ (_,destFloor) -> destFloor:tail up) (\ (startFloor,_) -> startFloor:down)) (take1pair floor) ++
          concatMap (put2chips (head up) (\ (_,destFloor) -> destFloor:tail up) (\ (startFloor,_) -> startFloor:down)) (take2chips floor) ++
          concatMap (put2gens (head up) (\ (_,destFloor) -> destFloor:tail up) (\ (startFloor,_) -> startFloor:down)) (take2gens floor))

take1chip :: Floor -> [(String,Floor)]
take1chip (chips,gens) = filter (ok . snd) [(t,(adjust (-1+) t chips,gens)) | (t,n) <- Data.Map.toList chips, n > 0]

take2chips :: Floor -> [((String,String),Floor)]
take2chips (chips,gens) = filter (ok . snd) $ [((t,t),(adjust (-2+) t chips,gens)) | (t,n) <- Data.Map.toList chips, n > 1] ++ [((t1,t2),(adjust (-1+) t1 (adjust (-1+) t2 chips),gens)) | (t1,n1) <- Data.Map.toList chips, n1 > 0, (t2,n2) <- Data.Map.toList chips, t2 > t1, n2 > 0]

take1gen :: Floor -> [(String,Floor)]
take1gen (chips,gens) = filter (ok . snd) [(t,(chips,adjust (-1+) t gens)) | (t,n) <- Data.Map.toList gens, n > 0]

take2gens :: Floor -> [((String,String),Floor)]
take2gens (chips,gens) = filter (ok . snd) $ [((t,t),(chips,adjust (-2+) t gens)) | (t,n) <- Data.Map.toList gens, n > 1] ++ [((t1,t2),(chips,adjust (-1+) t1 (adjust (-1+) t2 gens))) | (t1,n1) <- Data.Map.toList gens, n1 > 0, (t2,n2) <- Data.Map.toList gens, t2 > t1, n2 > 0]

take1pair :: Floor -> [(String,Floor)]
take1pair (chips,gens) = [(t,(adjust (-1+) t chips,adjust (-1+) t gens)) | (t,n) <- Data.Map.toList chips, n > 0 && gens!t > 0]

put1chip :: Floor -> ((Floor,Floor) -> [Floor]) -> ((Floor,Floor) -> [Floor]) -> (String,Floor) -> [State]
put1chip (chips,gens) makeUp makeDown (chip,srcFloor)
  | not $ ok destFloor = []
  | otherwise = [(makeUp (srcFloor,destFloor),makeDown (srcFloor,destFloor))]
  where
    destFloor = (adjust (+1) chip chips,gens)

put1gen :: Floor -> ((Floor,Floor) -> [Floor]) -> ((Floor,Floor) -> [Floor]) -> (String,Floor) -> [State]
put1gen (chips,gens) makeUp makeDown (gen,srcFloor)
  | not $ ok destFloor = []
  | otherwise = [(makeUp (srcFloor,destFloor),makeDown (srcFloor,destFloor))]
  where
    destFloor = (chips,adjust (+1) gen gens)

put1pair :: Floor -> ((Floor,Floor) -> [Floor]) -> ((Floor,Floor) -> [Floor]) -> (String,Floor) -> [State]
put1pair (chips,gens) makeUp makeDown (chip,srcFloor)
  | not $ ok destFloor = []
  | otherwise = [(makeUp (srcFloor,destFloor),makeDown (srcFloor,destFloor))]
  where
    destFloor = (adjust (+1) chip chips,adjust (+1) chip gens)

put2chips :: Floor -> ((Floor,Floor) -> [Floor]) -> ((Floor,Floor) -> [Floor]) -> ((String,String),Floor) -> [State]
put2chips (chips,gens) makeUp makeDown ((chip1,chip2),srcFloor)
  | not $ ok destFloor = []
  | otherwise = [(makeUp (srcFloor,destFloor),makeDown (srcFloor,destFloor))]
  where
    destFloor = (adjust (+1) chip1 $ adjust (+1) chip2 chips,gens)

put2gens :: Floor -> ((Floor,Floor) -> [Floor]) -> ((Floor,Floor) -> [Floor]) -> ((String,String),Floor) -> [State]
put2gens (chips,gens) makeUp makeDown ((gen1,gen2),srcFloor)
  | not $ ok destFloor = []
  | otherwise = [(makeUp (srcFloor,destFloor),makeDown (srcFloor,destFloor))]
  where
    destFloor = (chips,adjust (+1) gen1 $ adjust (+1) gen2 gens)

search :: Int -> Set State -> Set State -> Int
search nsteps seenStates states
  | any done states = nsteps
  | otherwise = search (nsteps+1) (seenStates `union` states) $ fromList $ concatMap (step seenStates) $ Data.Set.toList states

test :: ()
test
  | search 0 empty (fromList [parse testData]) /= 11 = error "a"
  | otherwise = ()
  where
    testData = "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\nThe second floor contains a hydrogen generator.\nThe third floor contains a lithium generator.\nThe fourth floor contains nothing relevant."

part1 :: IO Int
part1 = fmap (search 0 empty . fromList . (:[]) . parse) $ readFile "input/11.txt"

addPart2 :: State -> State
addPart2 (((chips,gens):upper),lower) = (((addExtra 1 chips,addExtra 1 gens):map noExtra upper),map noExtra lower)
  where
    addExtra n m = insert "elerium" n $ insert "dilithium" n m
    noExtra (c,g) = (addExtra 0 c,addExtra 0 g)

-- The brute-force search for part 2 takes an unsatisfactorily long time.

part2 :: IO Int
part2 = fmap (search 0 empty . fromList . (:[]) . addPart2 . parse) $ readFile "input/11.txt"
