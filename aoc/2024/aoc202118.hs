module AOC202118 where

import Control.Monad.State(State,evalState,get,put)
import Data.Char(ord)

import AOC

aoc = AOC {
    day="../../2021/input/18",
    aocTests=[
        AOCTest {
            testData=unlines [
                "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
                "[[[5,[2,8]],4],[5,[[9,9],0]]]",
                "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
                "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
                "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
                "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
                "[[[[5,4],[7,7]],8],[[8,3],8]]",
                "[[9,3],[[9,9],[6,[4,9]]]]",
                "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
                "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
                ],
            testResult=Just "4140",
            testResult2=Just "3993"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=const result,
        pcodeTest2=result2,
        pcodeResult=const result,
        pcodeResult2=result2
        }
    }

data Pair = Pair (Either Int Pair) (Either Int Pair)

instance Show Pair where
    show (Pair left right) =
        '[' : either show show left ++ ',' : either show show right ++ "]"

parse = map (evalState parsePair) . lines

parsePair :: State String Pair
parsePair = do
    ('[':subpair@(c:rest)) <- get
    left <-
        if c == '['
          then do
            put subpair
            pair <- parsePair
            return $ Right pair
          else do
            put rest
            return $ Left $ ord c - ord '0'
    (',':subpair@(c:rest)) <- get
    right <-
        if c == '['
          then do
            put subpair
            pair <- parsePair
            return $ Right pair
          else do
            put rest
            return $ Left $ ord c - ord '0'
    (']':rest) <- get
    put rest
    return $ Pair left right

reduce :: Pair -> Pair
reduce pair =
    maybe (maybe pair reduce (split pair)) reduceExplode (explode pair 0)
  where
    reduceExplode (_,Right explodedPair) = reduce explodedPair

explode :: Pair -> Int -> Maybe ((Int,Int),Either Int Pair)
explode (Pair (Left left) (Left right)) n
  | n < 4 = Nothing
  | otherwise = Just ((left,right),Left 0)
explode (Pair (Right left) (Left right)) n =
    fmap leftExploded $ explode left (n+1)
  where
    leftExploded ((l,r),left) = ((l,0),Right (Pair left (Left (right+r))))
explode (Pair (Left left) (Right right)) n =
    fmap rightExploded $ explode right (n+1)
  where
    rightExploded ((l,r),right) = ((0,r),Right (Pair (Left (left+l)) right))
explode (Pair (Right left) (Right right)) n =
    maybe tryRight (Just . leftExploded) $ explode left (n+1)
  where
    tryRight = fmap rightExploded $ explode right (n+1)

    leftExploded ((l,r),left) =
        ((l,0),Right (Pair left (propogateRight (Right right) r)))
    rightExploded ((l,r),right) =
        ((0,r),Right (Pair (propogateLeft (Right left) l) right))

    propogateLeft (Left n) l = Left (n+l)
    propogateLeft (Right (Pair left right)) l =
        Right (Pair left (propogateLeft right l))

    propogateRight (Left n) r = Left (n+r)
    propogateRight (Right (Pair left right)) r =
        Right (Pair (propogateRight left r) right)

split :: Pair -> Maybe Pair
split (Pair left right) =
    maybe tryRight (Just . flip Pair right . Right) $ splitItem left
  where
    tryRight = fmap (Pair left . Right) $ splitItem right

    splitItem (Right pair) = split pair
    splitItem (Left n)
      | n < 10 = Nothing
      | otherwise = Just (Pair (Left (n `div` 2)) (Left ((n+1) `div` 2)))

magnitude :: Pair -> Int
magnitude (Pair left right) =
    3*either id magnitude left + 2*either id magnitude right

add :: Pair -> Pair -> Pair
add a b = reduce (Pair (Right a) (Right b))

result (n:ns) = magnitude $ foldl add n ns

result2 ncpu ns = parallelMapReduce ncpu magnitude maximum
                                    [add a b | a <- ns, b <- ns]
