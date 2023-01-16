{-
--- Day 8: Space Image Format ---

The Elves' spirits are lifted when they realize you have an opportunity to
reboot one of their Mars rovers, and so they are curious if you would spend a
brief sojourn on Mars. You land your ship near the rover.

When you reach the rover, you discover that it's already in the process of
rebooting! It's just waiting for someone to enter a BIOS password. The Elf
responsible for the rover takes a picture of the password (your puzzle input)
and sends it to you via the Digital Sending Network.

Unfortunately, images sent via the Digital Sending Network aren't encoded with
any normal encoding; instead, they're encoded in a special Space Image Format.
None of the Elves seem to remember why this is the case. They send you the
instructions to decode it.

Images are sent as a series of digits that each represent the color of a single
pixel. The digits fill each row of the image left-to-right, then move downward
to the next row, filling rows top-to-bottom until every pixel of the image is
filled.

Each image actually consists of a series of identically-sized layers that are
filled in this way. So, the first digit corresponds to the top-left pixel of
the first layer, the second digit corresponds to the pixel to the right of that
on the same layer, and so on until the last digit, which corresponds to the
bottom-right pixel of the last layer.

For example, given an image 3 pixels wide and 2 pixels tall, the image data
123456789012 corresponds to the following image layers:

| Layer 1: 123
|          456
| 
| Layer 2: 789
|          012

The image you received is 25 pixels wide and 6 pixels tall.

To make sure the image wasn't corrupted during transmission, the Elves would
like you to find the layer that contains the fewest 0 digits. On that layer,
what is the number of 1 digits multiplied by the number of 2 digits?
-}

import Data.Map(Map,alter,empty,findWithDefault)

readForCheck :: Int -> Int -> String -> [Map Char Int]
readForCheck w h input
  | null input = []
  | otherwise = tabulate (take (w*h) input) : readForCheck w h (drop (w*h) input)
  where
    tabulate = foldr (alter (Just . maybe 1 (+1))) empty

check :: [Map Char Int] -> Int
check = snd . minimum . map counts
  where
    counts table = (count '0',count '1' * count '2')
      where count ch = findWithDefault 0 ch table

part1 :: IO Int
part1 = fmap (check . readForCheck 25 6 . filter (/= '\n')) $ readFile "input/08.txt"

parse :: Int -> Int -> String -> [String]
parse w h input
  | null input = []
  | otherwise = layer : parse w h rest
  where (layer,rest) = splitAt (w*h) input

merge :: String -> String -> String
merge top bottom = zipWith mergePixel top bottom
  where
    mergePixel '2' b = b
    mergePixel t _ = t

display :: Int -> String -> [String]
display w str
  | null str = []
  | otherwise = line : display w rest where (line,rest) = splitAt w str

part2 :: IO ()
part2 = do
    input <- readFile "input/08.txt"
    putStr $ unlines $ display 25 $ foldl merge (repeat '2') $ parse 25 6 $ filter (/= '\n') input
