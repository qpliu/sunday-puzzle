module AOC202512 where

import Data.List(transpose)
import Data.Map(Map,findWithDefault,fromList,insert)

import AOC

aoc = AOC {
    day="12",
    aocTests=[
        AOCTest {
            testData=unlines [
                "0:",
                "###",
                "##.",
                "##.",
                "",
                "1:",
                "###",
                "##.",
                ".##",
                "",
                "2:",
                ".##",
                "###",
                "##.",
                "",
                "3:",
                "##.",
                "###",
                "##.",
                "",
                "4:",
                "###",
                "#..",
                "###",
                "",
                "5:",
                "###",
                ".#.",
                "###",
                "",
                "4x4: 0 0 0 0 2 0",
                "12x5: 1 0 1 0 2 2",
                "12x5: 1 0 1 0 3 2"
            ],
            testResult=Nothing,
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

{- shapes in my input:

000 ..1 22. 333 4.4 555
0.. .11 222 .33 444 555
000 11. 2.2 ..3 4.4 5..

0 appears 12 to 71 times
1 appears 15 to 74 times
2 appears 13 to 74 times
3 appears 12 to 78 times
4 appears 11 to 72 times
5 appears 14 to 73 times

0 can be tiled by itself: 14 0s makes a 16x9
adding another tile extends it by 14x7 so 56 0s makes a 30x16
and 70 makes 30x16 + 16x7 or 30x16 + 14x9
the 71st can be a loose 3x3

000........***..
0000xxx......*..
0000xxxx|||***..
 000xxxx||||ooo.
  ***xxx||||oooo
  ****---|||oooo
  ****----___ooo
   ***----_.....
       ---___...

3 can be tiled by itself: 2 makes a 4x3
333
o33
oo3
ooo

4 5s and 2 4s: make a 8x7 tile
adding horizontally expands by 6, so 8 5s and 4 4s would be 14x7

555555
555555
5 4544 4
  444444
5.4544 4
555555
555555

2 1s and 1 3: make a 6x3 tile with 2 holes, holes are separated by 5

1.o333
11oo33
.11oo3

2 1s and 1 3 and 1 5: make a 8x3 tile with 1 holes, holes are separated by 7
or separated by alternating 6 and 8

551.o333
5511oo33
55511oo3

2 2s and 2 3s and 1 4: make a 5x7 tile with 2 holes, holes are separated by 6

22333
22233
24243
.444.
24243
22233
22333

4 by itself: 2 makes a 5x4 with 2 holes, holes are separated by 3
adding horizontally expands by 4, so 4 makes a 9x4 with 4 holes

444.444..
.4---4---
444-444-.
..---.---

2 1s, 2 3s, and 1 4: 9x4, spikes separated by alternating 2 and 5, or
spikes on opposite sides, making 9x5, spikes separated by 8

.1.....1.
311444113
331141133
333444333

4 1s, 4 3s, 4 4s, plus 1 anything: 9x9

333444333
331141133
311444113
414...414
444...444
414...414
311444113
331141133
333444333


1 2 and 1 5: 5x3 with a hole separated by 4 or, on both sides, separated by 9

22555
22255
2.255

14 2s and 14 5s: 28x9: this could be tiled, but can't be rectangularly tiled
without being uselessly large

   22555          +++ +
   22255*****     +++++888 8
55-2-255*****ddddd+++++88888
55---********ddddd=====88888
555--*****dddddddd=====!!!!!
=====*****ddddd========!!!!!
=====/////ddddd=====!!!!!!!!
= ===/////     =====!!!!!
     / ///          !!!!!

turns out that tilings aren't needed to get the answer
-}

data Shape =
    C | Slash | Four | Tri | H | Comma | Hex | Pent deriving (Eq,Ord,Show)

{-
 C: ### Slash: ##. Four: #.# Tri: ### H: #.# Comma: ### Hex: .## Pent: .#.
    #..        .##       ###      ##.    ###        ###      ###       ###
    ###        ..#       .##      #.     #.#        ..#      ##.       ###
-}

parseShape spec
  | matches ["###","#..","###"] = C
  | matches ["##.",".##","..#"] = Slash
  | matches ["#.#","###","##."] = Four
  | matches ["###","##.","#.."] = Tri
  | matches ["#.#","###","#.#"] = H
  | matches ["###","###","..#"] = Comma
  | matches [".##","###","##."] = Hex
  | matches [".#.","###","###"] = Pent
  where
    matches = (`elem` [spec,reverse spec,map reverse spec,
                       reverse (map reverse spec),
                       transpose spec,transpose (reverse spec),
                       transpose (map reverse spec),
                       transpose (reverse (map reverse spec))])

-- all shapes are 3x3 in my input and no shape has an unoccupied row or column
-- type Shape = (Integer,Integer,Integer) -- initially tried using bitsets

-- regions are (list of (width, height), numbers of each shape)
-- regions can be split into multiple rectangular areas
type Region = ([(Int,Int)],Map Shape Int)

parse :: String -> [Region]
parse = p . parseBlankLineSeparated
  where
    p inputs = map (parseRegion . parseInts) (last inputs)
      where
        shapes = map (parseShape . tail) (init inputs)
        parseRegion (w:h:counts) = ([(w,h)],fromList $ zip shapes counts)

result :: [Region] -> Int
result = length . filter fits

fits :: Region -> Bool
fits (areas,presents)
  | sum (map count3x3 areas) >= sum presents = True
  | tryCtilings = True
  | tryRingTilings areas [] = True
  | otherwise = False
  where
    count3x3 (h,w) = (h `div` 3)*(w `div` 3)
    count shape = findWithDefault 0 shape presents

    tryCtilings = False

    tryRingTilings [] _ = False
    tryRingTilings ((w,h):moreAreas) triedAreas
      | w < 9 || h < 9 = tryRingTilings moreAreas ((w,h):triedAreas)
      | w `mod` 9 == 0 && nRingTilings >= w `div` 9 &&
        let n = w `div` 9
        in  fits (((w,h-9) : moreAreas ++ triedAreas ++ take n (repeat (3,3))),
                  insert H (max 0 (count H - 4*n))
                   $ insert Slash (max 0 (count Slash - 4*n))
                   $ insert Tri (max 0 (count Tri - 4*n))
                   $ presents) =
          True
      | h `mod` 9 == 0 && nRingTilings >= h `div` 9 &&
        let n = h `div` 9
        in  fits ((w-9,h) : moreAreas ++ triedAreas ++ take n (repeat (3,3)),
                  insert H (max 0 (count H - 4*n))
                   $ insert Slash (max 0 (count Slash - 4*n))
                   $ insert Tri (max 0 (count Tri - 4*n))
                   $ presents) =
         True
      | otherwise = False
                 
    nRingTilings = (minimum [count H,count Slash,count Tri] + 3) `div` 4

{-
Also tried fitting shapes into rows of 5, starting from the left, so
the next shape needs to be at least one space to the right.

If, during the search, there is a 3x3 empty space to the left of the
current spot, the search has failed.

This turned out to be way too slow.
-}
  
result2 = const 0
