{-
--- Day 1: The Tyranny of the Rocket Equation ---

Santa has become stranded at the edge of the Solar System while delivering
presents to other planets! To accurately calculate his position in space,
safely align his warp drive, and return to Earth in time to save Christmas, he
needs you to bring him measurements from fifty stars.

Collect stars by solving puzzles. Two puzzles will be made available on each
day in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

The Elves quickly load you into a spacecraft and prepare to launch.

At the first Go / No Go poll, every Elf is Go until the Fuel Counter-Upper.
They haven't determined the amount of fuel required yet.

Fuel required to launch a given module is based on its mass. Specifically, to
find the fuel required for a module, take its mass, divide by three, round
down, and subtract 2.

For example:

 - For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to
   get 2.
 - For a mass of 14, dividing by 3 and rounding down still yields 4, so the
   fuel required is also 2.
 - For a mass of 1969, the fuel required is 654.
 - For a mass of 100756, the fuel required is 33583.

The Fuel Counter-Upper needs to know the total fuel requirement. To find it,
individually calculate the fuel needed for the mass of each module (your puzzle
input), then add together all the fuel values.

What is the sum of the fuel requirements for all of the modules on your
spacecraft?
-}

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

test :: ()
test
  | fuel 12 /= 2 = error "a"
  | fuel 14 /= 2 = error "b"
  | fuel 1969 /= 654 = error "c"
  | fuel 100756 /= 33583 = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map (fuel . read) . words) $ readFile "input/01.txt"

-- This gives the wrong answer due to integer division and multiplication
-- not being inverses.
totalFuel :: Int -> Int
totalFuel mass = max 0 (mass `div` 2 - 3)

totalFuel2 :: Int -> Int
totalFuel2 = sum . tail . takeWhile (> 0) . iterate fuel

test2 :: ()
test2
  | totalFuel2 14 /= 2 = error "a"
  | totalFuel2 1969 /= 966 = error "b"
  | totalFuel2 100756 /= 50346 = error "c"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (sum . map (totalFuel2 . read) . words) $ readFile "input/01.txt"
