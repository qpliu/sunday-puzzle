module AOC202325 where

import Data.Map(Map,alter,empty,findMax,findMin,mapWithKey,toList,(!))
import qualified Data.Map
import Data.Set(Set,delete,elems,insert,member,singleton)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2023/input/25",
    aocTests=[
        AOCTest {
            testData=unlines [
                "jqt: rhn xhk nvd",
                "rsh: frs pzl lsr",
                "xhk: hfx",
                "cmg: qnr nvd lhk bvb",
                "rhn: xhk bvb hfx",
                "bvb: xhk hfx",
                "pzl: lsr hfx nvd",
                "qnr: nvd",
                "ntq: jqt hfx bvb xhk",
                "nvd: lhk",
                "lsr: lhk",
                "rzs: qnr cmg lsr rsh",
                "frs: qnr lhk lsr"
                ],
            testResult=Just "54",
            testResult2=Nothing
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const (const ()),
        pcodeTest=result,
        pcodeTest2=const (const ()),
        pcodeResult=result,
        pcodeResult2=const (const ())
        }
    }

type Graph = (Map String (Int,Set Edge),Map String String)
type Edge = (String,String)

contractEdge :: (Map String (Int,Set Edge) -> (String,(Int,Set Edge)))
             -> (Set Edge -> Edge)
             -> Graph -> Graph
contractEdge chooseVertex chooseEdge (graph,mergers) = (graph2,mergers2)
  where
    (originalVertex1,originalVertex2) =
        chooseEdge (snd $ snd $ chooseVertex graph)
    mergedVertex1 = mergers!originalVertex1
    mergedVertex2 = mergers!originalVertex2
    v1 = min mergedVertex1 mergedVertex2
    v2 = max mergedVertex1 mergedVertex2
    (size1,edges1) = graph!v1
    (size2,edges2) = graph!v2

    updateMerger v | v == v2 = v1 | otherwise = v
    mergers2 = Data.Map.map updateMerger mergers

    mergeEdge (va,vb) edgeSet
      | member (vb,va) edgeSet = delete (vb,va) edgeSet
      | otherwise = insert (va,vb) edgeSet
    edges = foldr mergeEdge edges1 $ elems edges2
    graph2 = Data.Map.insert v1 (size1+size2,edges) $ Data.Map.delete v2 graph

contract :: (Map String (Int,Set Edge) -> (String,(Int,Set Edge)))
         -> (Set Edge -> Edge)
         -> Graph -> [(Int,Set Edge)]
contract chooseVertex chooseEdge g@(graph,_)
  | Data.Map.size graph == 2 = Data.Map.elems graph
  | otherwise =
      contract chooseVertex chooseEdge (contractEdge chooseVertex chooseEdge g)

parse :: String -> Graph
parse = addMerges . foldr build empty . concatMap (p . words) . lines
  where
    p (v:vs) = map ((,) (init v)) vs
    build (v1,v2) graph = alter (a (v1,v2)) v1 $ alter (a (v2,v1)) v2 graph
      where a edge = Just . maybe (singleton edge) (insert edge)
    addMerges graph = (mapWithKey (const ((,) 1)) graph,mapWithKey const graph)

isMinimumCut :: [(Int,Set Edge)] -> Bool
isMinimumCut ((_,edges):_) = Data.Set.size edges == 3

result ncpu g =
    product $ map fst $ head $ filter isMinimumCut
            $ parallelMapReduce ncpu makeCut concat
                ([(head . drop n . toList,maximum) | n <- [0..2]]
                 ++ [(findMax,head . drop n . elems) | n <- [0..3]])
  where
    makeCut (chooseV,chooseE) = [contract chooseV chooseE g]
