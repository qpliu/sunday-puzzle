import Data.Set(Set,empty,insert,member)
import System.Environment(getArgs,withArgs)

import AOC
import AOC202401
import AOC202402
import AOC202403
import AOC202404
import AOC202405
import AOC202406
import AOC202407
import AOC202408
import AOC202409
import AOC202410
import AOC202411
import AOC202412
import AOC202413
import AOC202414
import AOC202415
import AOC202416
import AOC202417
import AOC202418
import AOC202419

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
        r AOC202401.aoc,
        r AOC202402.aoc,
        r AOC202403.aoc,
        r AOC202404.aoc,
        r AOC202405.aoc,
        r AOC202406.aoc,
        r AOC202407.aoc,
        r AOC202408.aoc,
        r AOC202409.aoc,
        r AOC202410.aoc,
        r AOC202411.aoc,
        r AOC202412.aoc,
        r AOC202413.aoc,
        r AOC202414.aoc,
        r AOC202415.aoc,
        r AOC202416.aoc,
        r AOC202417.aoc,
        r AOC202418.aoc,
        r AOC202419.aoc
        ]
    putStrLn ("Total time: " ++ show (sum times))
  where
    r aoc
      | member (day aoc) includes = run aoc
      | member (day aoc) excludes = return $ fromInteger 0
      | includes == empty = run aoc
      | otherwise = return $ fromInteger 0
