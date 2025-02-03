module AOC2017 where

import Data.Bits(xor)
import Data.Char(ord)

import Control.Monad.ST(runST)
import Data.Vector.Unboxed.Mutable(swap,write)
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Vector.Unboxed(Vector,freeze,(!))
import qualified Data.Vector.Unboxed as V

knotSparse :: Int -> [Int] -> Vector Int
knotSparse nrounds lengths = runST $ do
    v <- VM.new 256
    sequence_ [write v i i | i <- [0..255]]
    knotRound v 0 0 (concat $ replicate nrounds lengths)
    freeze v
  where
    knotRound v curPos skipSize [] = return ()
    knotRound v curPos skipSize (len:lens) = do
        sequence_ [swap v ((curPos+l) `mod` 256) ((curPos+len-1-l) `mod` 256)
                   | l <- [0..len `div` 2 - 1]]
        knotRound v ((curPos + skipSize + len) `mod` 256) (skipSize+1) lens

knot :: String -> Vector Int
knot = dense . knotSparse 64 . (++ [17,31,73,47,23]) . map ord
  where
    dense v = V.fromList [
        foldr xor 0 $ map (v!) [0..15],    foldr xor 0 $ map (v!) [16..31],
        foldr xor 0 $ map (v!) [32..47],   foldr xor 0 $ map (v!) [48..63],
        foldr xor 0 $ map (v!) [64..79],   foldr xor 0 $ map (v!) [80..95],
        foldr xor 0 $ map (v!) [96..111],  foldr xor 0 $ map (v!) [112..127],
        foldr xor 0 $ map (v!) [128..143], foldr xor 0 $ map (v!) [144..159],
        foldr xor 0 $ map (v!) [160..175], foldr xor 0 $ map (v!) [176..191],
        foldr xor 0 $ map (v!) [192..207], foldr xor 0 $ map (v!) [208..223],
        foldr xor 0 $ map (v!) [224..239], foldr xor 0 $ map (v!) [240..255]
        ]
