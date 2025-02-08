module AOC201519 where

import Data.Char(isLower,isUpper)
import Data.List(groupBy)
import Data.Set(Set,elems,fromList,insert,size)
import qualified Data.Set
import Data.Map(Map,alter,empty,member,toList,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2015/input/19",
    aocTests=[
        AOCTest {
            testData=unlines [
                "e => H",
                "e => O",
                "H => HO",
                "H => OH",
                "O => HH",
                "",
                "HOHOHO"
                ],
            testResult=Just "7",
            testResult2=Just "6"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

toElements :: String -> [String]
toElements (a:rest@(b:rest2))
  | isUpper b = [a] : toElements rest
  | isLower b = [a,b] : toElements rest2
toElements [a]
  | isUpper a = [[a]]
toElements [] = []

addRule :: [String] -> Map String [[String]] -> Map String [[String]]
addRule [src,"=>",mol] =
    alter (Just . maybe [toElements mol] (toElements mol:)) src

parse = p . parseBlankLineSeparated
  where
    p [rules,[mol]] = (toElements mol,foldr addRule empty $ map words rules)

combinations :: ([String],Map String [[String]]) -> [[String]]
combinations ([],rules) = []
combinations ((src:rest),rules)
  | member src rules =
      map (src:) (combinations (rest,rules)) ++ map (++rest) (rules!src)
  | otherwise = map (src:) $ combinations (rest,rules)

result = size . fromList . combinations

invert :: Map String [[String]] -> (Map String [Int],Map [String] [String])
invert = toLists . foldr collect (empty,empty) . toList
  where
    toLists (lengths,byResult) =
        (Data.Map.map elems lengths,Data.Map.map elems byResult)

    collect (src,mols) tables = foldr (collect2 src) tables mols

    collect2 src mol@(atom:_) (lengths,byResult) =
        (alter (Just . maybe (fromList [length mol]) (insert (length mol)))
               atom lengths,
         alter (Just . maybe (fromList [src]) (insert src)) mol byResult)

addTerminators :: ([String],Map String [[String]])
               -> ((Set String,[String]),Map String [[String]])
addTerminators (mol,rules) = ((Data.Set.filter f $ fromList mol,mol),rules)
  where
    f atom = not (member atom rules || Data.Set.member atom results)
    results :: Set String
    results = fromList $ concatMap (concatMap init . snd) $ toList rules

parse2 = fmap invert . addTerminators . parse

reduce :: (Set String,(Map String [Int],Map [String] [String]))
       -> (Int,[String]) -> (Int,[String])
reduce tables@(terminators,(lengths,byResult)) (initialSteps,initialMol)
  | null mol2 = finish [(initialSteps,initialMol)]
  | otherwise = reduce tables $ r [(initialSteps,mol1 ++ [term])]
  where
    (mol1,mol2) = span (not . (`Data.Set.member` terminators)) initialMol
    (term:mol3) = mol2

    r ((steps,mol):rest)
      | term /= last mol = (steps,mol++mol3)
      | otherwise = r $ r1 (steps,mol) ++ rest

    r1 (steps,mol@(atom:rest))
      | not (member atom lengths) = map (fmap (atom:)) $ r1 (steps,rest)
      | otherwise =
          [(steps+1,src:remaining) | n <- lengths!atom,
                                     (dest,remaining) <- [splitAt n mol],
                                     member dest byResult,
                                     src <- byResult!dest, src /= "e"]
                ++ map (fmap (atom:)) (r1 (steps,rest))
    r1 _ = []

    finish ((steps,mol):rest)
      | mol == ["e"] = (steps,mol)
      | elem "e" mol = finish rest
      | otherwise = finish $ f1 (steps,mol) ++ rest
    finish a = error (show a)

    f1 (steps,mol@(atom:rest))
      | not (member atom lengths) = map (fmap (atom:)) $ f1 (steps,rest)
      | otherwise =
          [(steps+1,src:remaining) | n <- lengths!atom,
                                     (dest,remaining) <- [splitAt n mol],
                                     member dest byResult,
                                     src <- byResult!dest]
                ++ map (fmap (atom:)) (f1 (steps,rest))
    f1 _ = []

result2 ((terminators,mol),tables) = fst $ reduce (terminators,tables) (0,mol)
