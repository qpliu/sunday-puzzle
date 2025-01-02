module AOC202323 where

import Data.Array(Array,bounds,inRange,(!))
import Data.Map(Map)
import qualified Data.Map
import Data.Set(Set,empty,fromList,insert,member,size)

import AOC

aoc = AOC {
    day="../../2023/input/23",
    aocTests=[
        AOCTest {
            testData=unlines [
                "#.#####################",
                "#.......#########...###",
                "#######.#########.#.###",
                "###.....#.>.>.###.#.###",
                "###v#####.#v#.###.#.###",
                "###.>...#.#.#.....#...#",
                "###v###.#.#.#########.#",
                "###...#.#.#.......#...#",
                "#####.#.#.#######.#.###",
                "#.....#.#.#.......#...#",
                "#.#####.#.#.#########v#",
                "#.#...#...#...###...>.#",
                "#.#.#v#######v###.###v#",
                "#...#.>.#...>.>.#.###.#",
                "#####v#.#.###v#.#.###.#",
                "#.....#...#...#.#.#...#",
                "#.#########.###.#.#.###",
                "#...###...#...#...#.###",
                "###.###.#.###v#####v###",
                "#...#...#.#.>.>.#.>.###",
                "#.###.###.#.###.#.#v###",
                "#.....###...###...#...#",
                "#####################.#"
                ],
            testResult=Just "94",
            testResult2=Just "154"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const (parse True),
        pcodeParse2=const (parse False),
        pcodeTest=result,
        pcodeTest2=result,
        pcodeResult=result,
        pcodeResult2=result
        }
    }

parse :: Bool -> String -> Map (Int,Int) [(Int,Maybe (Int,Int))]
parse slippery = toGraph slippery . parse2da

toGraph :: Bool -> Array (Int,Int) Char
        -> Map (Int,Int) [(Int,Maybe (Int,Int))]
toGraph slippery grid = dfs Data.Map.empty [start]
  where
    range@(_,(xmax,ymax)) = bounds grid
    inBounds = inRange range
    start = (1,0)
    end = (xmax-1,ymax)

    dfs graph [] = graph
    dfs graph (src@(srcX,srcY):queue)
      | Data.Map.member src graph = dfs graph queue
      | otherwise = dfs (Data.Map.insert src dests graph)
                        ([dest | (_,maybeDest) <- dests,
                                 maybe False (const True) maybeDest,
                                 Just dest <- [maybeDest]] ++ queue)
      where
        dests
          -- Passing up the turn to the end means there's no way
          -- to reach the end without passing through here twice,
          -- so remove all other exits.
          | any isEnd allDests = filter isEnd allDests
          | otherwise = allDests
        isEnd = maybe True (const False) . snd
        allDests = [dest | maybeDest <- map walk1 [((1,0),'>'),((0,1),'v'),
                                                   ((-1,0),'<'),((0,-1),'^')],
                           maybe False (const True) maybeDest,
                           Just dest <- [maybeDest]]

        walk1 (dxy@(dx,dy),slope)
          | not (inBounds (srcX+dx,srcY+dy)) = Nothing
          | grid!(srcX+dx,srcY+dy) == '#' = Nothing
          | slippery && grid!(srcX,srcY) /= '.'
                     && grid!(srcX,srcY) /= slope = Nothing
          | otherwise = walk 1 (srcX+dx,srcY+dy) (dx,dy)

        walk nsteps xy@(x,y) dxy@(dx,dy)
          | xy == end = Just (nsteps,Nothing)
          | xy == start || length exits == 0 = Nothing
          | length exits == 1 =
              head [walk (nsteps+1) (x+ex,y+ey) exy | exy@(ex,ey) <- exits]
          | otherwise = Just (nsteps,Just xy)
          where
            exits = [exit | (exit@(ex,ey),slope) <-
                                [((1,0),'>'),((0,1),'v'),
                                 ((-1,0),'<'),((0,-1),'^')],
                            (ex,ey) /= (-dx,-dy),
                            grid!(x+ex,y+ey) /= '#',
                            not slippery || grid!xy == '.' || grid!xy == slope]

result ncpu graph = maxPath empty 0 (1,0)
  where
    maxPath visited nsteps xy = maximum $ 0 : forks
      where
        forks = [maybe (nsteps+nsteps1)
                       (maxPath (insert xy visited) (nsteps+nsteps1))
                       dest
                 | (nsteps1,dest) <- graph Data.Map.! xy,
                   maybe True (not . (`member` visited)) dest]
