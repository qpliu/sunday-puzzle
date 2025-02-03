module AOC201707 where

import Data.List(nub)
import Data.Map(Map,alter,elems,empty,fromList,insert,member,size,toList,(!))
import Data.Set(Set,difference)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2017/input/07",
    aocTests=[
        AOCTest {
            testData=unlines [
                "pbga (66)",
                "xhth (57)",
                "ebii (61)",
                "havc (66)",
                "ktlj (57)",
                "fwft (72) -> ktlj, cntj, xhth",
                "qoyq (66)",
                "padx (45) -> pbga, havc, qoyq",
                "tknk (41) -> ugml, padx, fwft",
                "jptl (61)",
                "ugml (68) -> gyxo, ebii, jptl",
                "gyxo (61)",
                "cntj (57)"
                ],
            testResult=Just $ show "tknk",
            testResult2=Just "60"
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

parse = foldr collect (Data.Set.empty,Data.Set.empty) . map words . lines
  where
    collect (tower:_:"->":subs) (towers,subtowers) =
        (Data.Set.insert tower towers,
         foldr Data.Set.insert subtowers $ map (filter (/= ',')) subs)
    collect _ (towers,subtowers) = (towers,subtowers)

result = minimum . uncurry difference

parse2 :: String -> Map String ((Int,Int),[(String,Int)])
parse2 input = tower
  where
    tower = fromList $ map (p . words) $ lines input
    p [t,weight] = (t,((w,w),[]))
      where w = read weight
    p (t:weight:"->":subs) = (t,((w,w+sum (map snd s)),s))
      where
        w = read weight
        s = [(sub,snd $ fst $ tower!sub) | sub <- map (filter (/= ',')) subs]

unbalanced :: ((Int,Int),[(String,Int)]) -> [(String,Int)]
unbalanced (_,weights)
  | size byWeight < 2 = []
  | otherwise = [(wrong,rightWeight-wrongWeight)]
  where
    byWeight = foldr collect empty weights
    collect (t,w) = alter (Just . maybe [t] (t:)) w
    [(rightWeight,_)] = filter ((> 1) . length . snd) $ toList byWeight
    [(wrongWeight,[wrong])] = filter ((== 1) . length . snd) $ toList byWeight

findBalance :: Map String ((Int,Int),[(String,Int)]) -> [(String,Int)] -> Int
findBalance tower ((t,delta):rest)
  | null (unbalanced tdata) = tweight+delta
  | otherwise = findBalance tower rest
  where tdata@((tweight,_),_) = tower!t

result2 tower = findBalance tower $ concatMap unbalanced $ elems tower
