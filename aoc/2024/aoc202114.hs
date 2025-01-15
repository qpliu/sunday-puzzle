module AOC202114 where

import Data.List(sort)
import Data.Map(Map,adjust,elems,fromList,insert,keys,toList,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2021/input/14",
    aocTests=[
        AOCTest {
            testData=unlines [
                "NNCB",
                "",
                "CH -> B",
                "HH -> N",
                "CB -> H",
                "NH -> C",
                "HB -> C",
                "HC -> B",
                "HN -> C",
                "NN -> C",
                "BH -> H",
                "NC -> B",
                "NB -> B",
                "BN -> B",
                "BB -> N",
                "BC -> B",
                "CC -> N",
                "CN -> C"
                ],
            testResult=Just "1588",
            testResult2=Just "2188189693529"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 10,
        codeTest2=result 40,
        codeResult=result 10,
        codeResult2=result 40
        }
    }

parse = p . lines
  where
    p (template:"":rules) = (last template,polymer,ruleTable)
      where
        ruleTable = fromList $ map toRuleTable rules
        toRuleTable (a:b:' ':'-':'>':' ':c:_) = ((a,b),c)

        polymer = foldr (adjust (1+)) (Data.Map.map (const 0) ruleTable)
                                      (zip template (tail template))

applyRules :: (Char,Map (Char,Char) Int,Map (Char,Char) Char)
           -> (Char,Map (Char,Char) Int,Map (Char,Char) Char)
applyRules (end,polymer,rules) = (end,nextPolymer,rules)
  where
    nextPolymer = foldr rule (Data.Map.map (const 0) polymer) $ toList polymer

    rule (ab@(a,b),n) polymer = adjust (+n) (a,c) $ adjust (+n) (c,b) polymer
      where c = rules!ab

metric :: (Char,Map (Char,Char) Int,Map (Char,Char) Char) -> Int
metric (end,polymer,_) = last quantities - head quantities
  where
    quantities =
        sort $ elems $ foldr collect initialQuantities $ toList polymer
    initialQuantities =
        insert end 1 $ fromList $ map (fmap (const 0)) $ keys polymer
    collect ((a,_),n) = adjust (+n) a

result n = metric . head . drop n . iterate applyRules
