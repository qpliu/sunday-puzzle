module AOC202508 where

import Data.List(sort)
import Data.Map(delete,empty,member,insert,toList,(!))
import qualified Data.Map

{-
import Control.Monad.ST(runST)
import Data.List(sort)
import Data.Vector.Unboxed((!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
-}

import AOC

aoc = AOC {
    day="08",
    aocTests=[
        AOCTest {
            testData=unlines [
                "162,817,812",
                "57,618,57",
                "906,360,560",
                "592,479,940",
                "352,342,300",
                "466,668,158",
                "542,29,236",
                "431,825,988",
                "739,650,466",
                "52,470,668",
                "216,146,977",
                "819,987,18",
                "117,168,530",
                "805,96,715",
                "346,949,466",
                "970,615,88",
                "941,993,340",
                "862,61,35",
                "984,92,344",
                "425,690,689"
            ],
            testResult=Just "40",
            testResult2=Just "25272"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 10,
        codeTest2=result2,
        codeResult=result 1000,
        codeResult2=result2
        }
    }

parse = toInput . toCoords . parseInts
  where
    toCoords (x:y:z:rest) = (x,y,z) : toCoords rest
    toCoords _ = []
    toInput coords = (length coords,sort (toPairsWithDist2 coords))
    toPairsWithDist2 (a:rest) =
        -- 200000000 is a heuristic to speed up the calculation
        -- by throwing away the most distant pairs.  Making it
        -- larger makes the calculation slower.  Making it too
        -- small makes the calculation give the wrong answer.
        [(d2,a,b) | b <- rest, d2 <- [dist2 a b], d2 < 200000000]
            ++ toPairsWithDist2 rest
    toPairsWithDist2 _ = []
    toPairWithDist2 a b = (dist2 a b,a,b)

dist2 (x1,y1,z1) (x2,y2,z2) = dx*dx + dy*dy + dz*dz
  where
    dx = x1-x2
    dy = y1-y2
    dz = z1-z2

result npairs (_,input) =
    product $ take 3 $ reverse $ sort $ map snd $ toList sizes
  where
    (sizes,circuits) = foldr collect (empty,empty) $ take npairs input
    collect (_,a,b) (sizes,circuits)
      | havea && haveb && ak == bk = (sizes,circuits)
      | havea && haveb =
          (insert ak (sizes!ak + sizes!bk) (delete bk sizes),
           Data.Map.map (\ k -> if k == bk then ak else k) circuits)
      | havea = (insert ak (sizes!ak + 1) sizes,insert b ak circuits)
      | haveb = (insert bk (sizes!bk + 1) sizes,insert a bk circuits)
      | otherwise = (insert a 2 sizes,insert b a (insert a a circuits))
      where
        havea = member a circuits
        haveb = member b circuits
        ak = circuits!a
        bk = circuits!b

result2 (nboxes,input) = collect empty empty input
  where
    collect sizes circuits ((_,a@(ax,_,_),b@(bx,_,_)):rest)
      | havea && haveb && ak == bk = collect sizes circuits rest
      | havea && haveb && asize + bsize == nboxes = ax*bx
      | havea && haveb =
            collect (insert ak (asize+bsize) (delete bk sizes))
                    (Data.Map.map (\ k -> if k == bk then ak else k) circuits)
                    rest
      | havea && asize+1 == nboxes = ax*bx
      | havea = collect (insert ak (asize + 1) sizes)
                        (insert b ak circuits)
                        rest
      | haveb && bsize+1 == nboxes = ax*bx
      | haveb = collect (insert bk (bsize + 1) sizes)
                        (insert a bk circuits)
                        rest
      | otherwise = collect (insert a 2 sizes)
                            (insert b a (insert a a circuits))
                            rest
      where
        havea = member a circuits
        haveb = member b circuits
        ak = circuits!a
        bk = circuits!b
        asize = sizes!ak
        bsize = sizes!bk

{-
-- using mutable vectors is slightly slower above approach

parse :: String -> (V.Vector Int,[(Int,Int)])
parse = toInput . zip [0..] . toCoords . parseInts
  where
    toCoords (x:y:z:rest) = (x,y,z) : toCoords rest
    toCoords _ = []
    toInput coords =
        (V.fromList (map getX coords),map snd (sort (toPairs coords)))
    toPairs ((ai,axyz):rest) =
        -- 200000000 is a heuristic to speed up the calculation
        -- by throwing away the most distant pairs.  Making it
        -- larger makes the calculation slower.  Making it too
        -- small makes the calculation give the wrong answer.
        [(d2,(ai,bi)) | (bi,bxyz) <- rest,
                        d2 <- [dist2 axyz bxyz],d2 < 200000000]
            ++ toPairs rest
    toPairs _ = []
    getX (_,(x,_,_)) = x

dist2 (x1,y1,z1) (x2,y2,z2) = dx*dx + dy*dy + dz*dz
  where
    dx = x1-x2
    dy = y1-y2
    dz = z1-z2

result :: Int -> (V.Vector Int,[(Int,Int)]) -> Int
result npairs (xs,pairs) = runST $ do
    sizes <- VM.replicate (V.length xs) 0
    circuits <- V.thaw $ V.generate (V.length xs) id
    mapM_ (connect sizes circuits) (take npairs pairs)
    fmap (product . take 3 . reverse . sort . V.toList) (V.freeze sizes)

connect sizes circuits (a,b) = do
    ac <- VM.read circuits a
    bc <- VM.read circuits b
    if ac == bc
      then return 0
      else do
        as <- VM.read sizes ac
        bs <- VM.read sizes bc
        if as > 0 && bs > 0 then do
            VM.write sizes ac (as+bs)
            VM.write sizes bc 0
            mapM_ (moveConnection circuits bc ac) [0 .. VM.length circuits - 1]
            return (as+bs)
        else if as > 0 then do
            VM.write sizes ac (as+1)
            VM.write circuits b ac
            return (as+1)
        else if bs > 0 then do
            VM.write sizes bc (bs+1)
            VM.write circuits a bc
            return (bs+1)
        else do
            VM.write sizes ac 2
            VM.write circuits bc ac
            return 2
  where
    moveConnection circuits bc ac i = do
      ic <- VM.read circuits i
      if ic == bc
        then VM.write circuits i ac
        else return ()

result2 :: (V.Vector Int,[(Int,Int)]) -> Int
result2 (xs,pairs) = runST $ do
    sizes <- VM.replicate (V.length xs) 0
    circuits <- V.thaw $ V.generate (V.length xs) id
    makeConnections sizes circuits
  where
    nboxes = V.length xs
    makeConnections sizes circuits = conn pairs
      where
        conn ((a,b):rest) = do
            size <- connect sizes circuits (a,b)
            if size == nboxes
              then return (xs!a * xs!b)
              else conn rest
-}
