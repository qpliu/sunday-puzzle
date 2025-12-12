import Data.Set(Set,empty,insert,member)
import System.Environment(getArgs,withArgs)

import AOC
import AOC202501
import AOC202502
import AOC202503
import AOC202504
import AOC202505
import AOC202506
import AOC202507
import AOC202508
import AOC202509
import AOC202510
import AOC202511
import AOC202512

main :: IO ()
main = getArgs >>= execute . foldr parseArg (empty,empty)

parseArg :: String -> (Set String,Set String) ->  (Set String,Set String)
parseArg ('-':t@[_]) (includes,excludes) = (includes,insert ('0':t) excludes)
parseArg ('-':t@[_,_]) (includes,excludes) = (includes,insert t excludes)
parseArg (t@[_]) (includes,excludes) = (insert ('0':t) includes,excludes)
parseArg (t@[_,_]) (includes,excludes) = (insert t includes,excludes)
parseArg _ (includes,excludes) = (includes,excludes)

execute :: (Set String,Set String) -> IO ()
execute (includes,excludes) = do
    times <- sequence [
        r AOC202501.aoc,
        r AOC202502.aoc,
        r AOC202503.aoc,
        r AOC202504.aoc,
        r AOC202505.aoc,
        r AOC202506.aoc,
        r AOC202507.aoc,
        r AOC202508.aoc,
        r AOC202509.aoc,
        r AOC202510.aoc,
        r AOC202511.aoc,
        r AOC202512.aoc
        ]
    putStrLn ("Total time: " ++ show (sum times))
  where
    r aoc
      | member (day aoc) includes = run aoc
      | member (day aoc) excludes = return $ fromInteger 0
      | includes == empty = run aoc
      | otherwise = return $ fromInteger 0
