import Data.Char(isDigit)
import Data.Map(Map,alter,elems,empty,findWithDefault,(!))

val :: Map String Int -> String -> Int
val regs token
  | head token == '-' || isDigit (head token) = read token
  | otherwise = findWithDefault 0 token regs

modify :: Map String Int -> String -> (Int -> Int) -> Map String Int
modify regs reg doModify = alter (Just . doModify . maybe 0 id) reg regs

cond :: Map String Int -> [String] -> Bool
cond regs (a:op:b:_) = getOp op (val regs a) (val regs b)
  where
    getOp ">" = (>)
    getOp "<" = (<)
    getOp ">=" = (>=)
    getOp "<=" = (<=)
    getOp "==" = (==)
    getOp "!=" = (/=)

interp :: Map String Int -> [String] -> Map String Int
interp regs (reg:op:arg:"if":rest)
  | cond regs rest = modify regs reg (getOp op (val regs arg))
  | otherwise = regs
  where
    getOp "dec" x = (negate x +)
    getOp "inc" x = (x +)

run :: String -> Map String Int
run s = foldl interp empty $ map words $ lines s

testData :: String
testData = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"

test :: ()
test
  | maximum (elems $ run testData) /= 1 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (maximum . elems . run) $ readFile "input/08.txt"

interp2 :: (Int,Map String Int) -> [String] -> (Int,Map String Int)
interp2 (maxval,regs) (reg:op:arg:"if":rest)
  | cond regs rest = (max maxval (newregs!reg),newregs)
  | otherwise = (maxval,regs)
  where
    newregs = modify regs reg (getOp op (val regs arg))
    getOp "dec" x = (negate x +)
    getOp "inc" x = (x +)

run2 :: String -> Int
run2 s = fst $ foldl interp2 (0,empty) $ map words $ lines s

test2 :: ()
test2
  | run2 testData /= 10 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap run2 $ readFile "input/08.txt"
