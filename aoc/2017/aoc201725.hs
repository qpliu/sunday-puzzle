import Data.Map(Map,fromList,(!))

data Blueprint = Blueprint String Int (Map (String,Int) Instruction) deriving Show

data Instruction = Instruction Int Bool String deriving Show

parse :: String -> Blueprint
parse = parseBegin . words
  where
    parseBegin ("Begin":"in":"state":begin:rest) = parseDiagnostic (filter (/= '.') begin) rest
    parseDiagnostic begin ("Perform":"a":"diagnostic":"checksum":"after":steps:_:rest) = Blueprint begin (read steps) $ fromList $ parseState rest
    parseState ("In":"state":state:rest) = parseIf (filter (/= ':') state) rest
    parseIf _ [] = []
    parseIf state ("If":"the":"current":"value":"is":val:rest) = parseWrite (state,(read (filter (/= ':') val))) rest
    parseIf _ tokens = parseState tokens
    parseWrite key ("-":"Write":"the":"value":val:rest) = parseMove key (read (filter (/= '.') val)) rest
    parseMove key val ("-":"Move":"one":"slot":"to":"the":move:rest) = parseContinue key val (move == "right.") rest
    parseContinue key@(state,_) val move ("-":"Continue":"with":"state":next:rest) = (key,Instruction val move (filter (/= '.') next)) : parseIf state rest

data State = State String Int [Int] [Int] deriving Show

step :: Map (String,Int) Instruction -> State -> State
step instructions (State state checksum left right) =
    exec (instructions!(state,oldVal))
  where
    oldVal = sum (take 1 left)
    exec (Instruction val True next) = State next (checksum+val-oldVal) (sum (take 1 right):val:drop 1 left) (drop 1 right)
    exec (Instruction val False next) = State next (checksum+val-oldVal) (drop 1 left) (val:right)

getChecksum :: State -> Int
getChecksum (State _ checksum left right) = checksum

run :: String -> State
run s = head $ drop nsteps $ iterate (step instructions) (State state 0 [] [])
  where
    Blueprint state nsteps instructions = parse s

testData :: String
testData = "Begin in state A.\nPerform a diagnostic checksum after 6 steps.\n\nIn state A:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state B.\n  If the current value is 1:\n    - Write the value 0.\n    - Move one slot to the left.\n    - Continue with state B.\n\nIn state B:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the left.\n    - Continue with state A.\n  If the current value is 1:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state A."

test :: ()
test
  | getChecksum (run testData) /= 3 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (getChecksum . run) $ readFile "input/25.txt"
