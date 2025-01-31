module AOC201808 where

import Control.Monad.State(State,evalState,get,put)

import AOC

aoc = AOC {
    day="../../2018/input/08",
    aocTests=[
        AOCTest {
            testData=unlines [
                "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
                ],
            testResult=Just "138",
            testResult2=Just "66"
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

data Node = Node [Node] [Int]

parseNode :: State [Int] Node
parseNode = do
    (nchildren:nmeta:rest) <- get
    put rest
    children <- sequence $ replicate nchildren parseNode
    (meta,rest) <- fmap (splitAt nmeta) get
    put rest
    return $ Node children meta

parse = evalState parseNode . parseInts

metaSum :: Node -> Int
metaSum (Node children meta) = sum (map metaSum children) + sum meta

result = metaSum

value :: Node -> Int
value (Node children meta)
  | null children = sum meta
  | otherwise = sum $ map (value . (children!!)) (map pred $ filter valid meta)
  where
    valid i = 0 < i && i <= nchildren
    nchildren = length children

result2 = value
