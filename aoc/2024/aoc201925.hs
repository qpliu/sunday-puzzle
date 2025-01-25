module AOC201925 where

import Data.Char(chr,ord)
import Data.Vector.Unboxed(Vector)

import AOC
import AOC2019

aoc = AOC {
    day="../../2019/input/25",
    aocTests=[],
    aocCode=Code {
        codeParse=parseIntCode,
        codeParse2=parseIntCode,
        codeTest=undefined,
        codeTest2=undefined,
        codeResult=result,
        codeResult2=return ()
        }
    }

interactIntCode :: (state -> String -> (state,String)) -> state -> String
                -> Vector Int -> String
interactIntCode makeInput initialState initialInput =
    intCodeIO (IntCodeIO (continue initialState [])) (map ord initialInput)
  where
    continue state outBuffer (Just output) input Nothing =
        map chr (reverse (output:outBuffer))
    continue state outBuffer (Just output) input (Just cont) =
        cont (IntCodeIO (continue state (output:outBuffer))) input
    continue state outBuffer Nothing input@(_:_) (Just cont) =
        cont (IntCodeIO (continue state outBuffer)) input
    continue state outBuffer Nothing [] (Just cont) =
        cont (IntCodeIO (continue newState [])) (map ord newInput)
      where
        (newState,newInput) = makeInput state (map chr (reverse outBuffer))

findWeight (item:items) text
  | elem "heavier" $ words text =
      (items,unlines ["take " ++ item, "drop " ++ head items, "south"])
  | null items = (items,"south\n")
  | otherwise = (items,unlines ["drop " ++ head items, "south"])

result = extract . interactIntCode findWeight items script
  where
    extract = head . drop 1 . dropWhile (/= "typing") . words

    script = unlines [
        "east",
        "south",
        "south",
        "take hologram",
        "north",
        "north",
        "west",
        "south",
        "take mouse",
        "east",
        "take shell",
        "west",
        "west",
        "take whirled peas",
        "east",
        "north",
        "west",
        "north",
        "north",
        "west",
        "take semiconductor",
        "east",
        "south",
        "west",
        "south",
        "take hypercube",
        "north",
        "east",
        "south",
        "west",
        "take antenna",
        "south",
        "take spool of cat6",
        "north",
        "west",
        "south",
        "drop spool of cat6"
        ]

    items = [
        "spool of cat6",
        "hologram",
        "shell",
        "antenna",
        "semiconductor",
        "whirled peas",
        "mouse",
        "hypercube"
        ]
