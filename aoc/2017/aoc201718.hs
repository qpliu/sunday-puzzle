import Data.Char(isDigit)
import Data.Map(Map,empty,findWithDefault,insert)
import qualified Data.Map

val :: Map String Int -> String -> Int
val regs token
  | head token == '-' || isDigit (head token) = read token
  | otherwise = findWithDefault 0 token regs

interp :: ([Int],Map String Int) -> ([[String]],[[String]]) -> ([Int],Map String Int)
interp (recovered,regs) ([],_) = (recovered,regs)
interp (recovered,regs) (insn:forward,backward) = interp1 insn
  where
    v = val regs
    interp1 ("snd":x:_) = interp (recovered,insert "snd" (v x) regs) (forward,insn:backward)
    interp1 ("set":x:y:_) = interp (recovered,insert x (v y) regs) (forward,insn:backward)
    interp1 ("add":x:y:_) = interp (recovered,insert x (v x + v y) regs) (forward,insn:backward)
    interp1 ("mul":x:y:_) = interp (recovered,insert x (v x * v y) regs) (forward,insn:backward)
    interp1 ("mod":x:y:_) = interp (recovered,insert x (v x `mod` v y) regs) (forward,insn:backward)
    interp1 ("rcv":x:_)
      | v x == 0 = interp (recovered,regs) (forward,insn:backward)
      | otherwise = (v "snd":recovered,regs)
    interp1 ("jgz":x:y:_)
      | v x <= 0 = interp (recovered,regs) (forward,insn:backward)
      | v y >= 0 = interp (recovered,regs) (nextForward,reverse skipForward ++ backward)
      | (-(v y)) >= length backward = (recovered,regs)
      | otherwise = interp (recovered,regs) (reverse skipBackward++insn:forward,nextBackward)
      where (skipForward,nextForward) = splitAt (v y) (insn:forward)
            (skipBackward,nextBackward) = splitAt (-(v y)) backward
    interp1 _ = error (show insn)

parse :: String -> ([[String]],[[String]])
parse s = (map words $ lines s,[])

testData :: String
testData = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"

test :: ()
test
  | fst (interp ([],empty) (parse testData)) /= [4] = error "a"
  | otherwise = ()

part1 :: IO [Int]
part1 = fmap (fst . interp ([],empty) . parse) $ readFile "input/18.txt"

type State = ([Int],[Int],Map String Int,[[String]],[[String]])

interp2 :: State -> State
interp2 (rcv,snd,regs,[],_) = (rcv,snd,regs,[],[])
interp2 (rcv,snd,regs,insn:forward,backward) = interp1 insn
  where
    v = val regs
    interp1 ("snd":x:_) = interp2 (rcv,(v x):snd,regs,forward,insn:backward)
    interp1 ("set":x:y:_) = interp2 (rcv,snd,insert x (v y) regs,forward,insn:backward)
    interp1 ("add":x:y:_) = interp2 (rcv,snd,insert x (v x + v y) regs,forward,insn:backward)
    interp1 ("mul":x:y:_) = interp2 (rcv,snd,insert x (v x * v y) regs,forward,insn:backward)
    interp1 ("mod":x:y:_) = interp2 (rcv,snd,insert x (v x `mod` v y) regs,forward,insn:backward)
    interp1 ("rcv":x:_)
      | null rcv = (rcv,snd,regs,insn:forward,backward)
      | otherwise = interp2 (tail rcv,snd,insert x (head rcv) regs,forward,insn:backward)
    interp1 ("jgz":x:y:_)
      | v x <= 0 = interp2 (rcv,snd,regs,forward,insn:backward)
      | v y >= 0 = interp2 (rcv,snd,regs,nextForward,reverse skipForward ++ backward)
      | (-(v y)) >= length backward = (rcv,snd,regs,[],[])
      | otherwise = interp2 (rcv,snd,regs,reverse skipBackward++insn:forward,nextBackward)
      where (skipForward,nextForward) = splitAt (v y) (insn:forward)
            (skipBackward,nextBackward) = splitAt (-(v y)) backward
    interp1 _ = error (show insn)

init2 :: String -> (Int,(State,State))
init2 s = (0,(([],[],empty,insns,[]),([],[],insert "p" 1 empty,insns,[])))
  where insns = map words $ lines s

step2 :: (Int,(State,State)) -> (Int,(State,State))
step2 (sendCount,(state0@(rcv0,snd0,regs0,insns0,back0),state1@(rcv1,snd1,regs1,insns1,back1)))
  | not (null snd0) || not (null snd1) =
    (sendCount+length snd1,((rcv0++reverse snd1,[],regs0,insns0,back0),(rcv1++reverse snd0,[],regs1,insns1,back1)))
  | otherwise = (sendCount,(interp2 state0,interp2 state1))

terminated :: (Int,(State,State)) -> Bool
terminated (_,((rcv0,snd0,_,insns0,_),(rcv1,snd1,_,insns1,_))) =
    null snd0 && null snd1 && (null insns0 || (null rcv0 && (take 1 (head insns0) == ["rcv"]))) && (null insns1 || (null rcv1 && (take 1 (head insns1) == ["rcv"])))

run2 :: (Int,(State,State)) -> (Int,(State,State))
run2 state
  | terminated state = state
  | otherwise = run2 (step2 state)

testData2 :: String
testData2 = "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d"

test2 :: ()
test2
  | fst (run2 $ init2 testData2) /= 3 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (fst . run2 . init2) $ readFile "input/18.txt"
