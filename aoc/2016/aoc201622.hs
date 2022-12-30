{-
--- Day 22: Grid Computing ---

You gain access to a massive storage cluster arranged in a grid; each storage
node is only connected to the four nodes directly adjacent to it (three if the
node is on an edge, two if it's in a corner).

You can directly access data only on node /dev/grid/node-x0-y0, but you can
perform some limited actions on the other nodes:

 - You can get the disk usage of all nodes (via df). The result of doing this
   is in your puzzle input.
 - You can instruct a node to move (not copy) all of its data to an adjacent
   node (if the destination node has enough space to receive the data). The
   sending node is left empty after this operation.

Nodes are named by their position: the node named node-x10-y10 is adjacent to
nodes node-x9-y10, node-x11-y10, node-x10-y9, and node-x10-y11.

Before you begin, you need to understand the arrangement of data on these
nodes. Even though you can only move data between directly connected nodes,
you're going to need to rearrange a lot of the data to get access to the data
you need. Therefore, you need to work out how you might be able to shift data
around.

To do this, you'd like to count the number of viable pairs of nodes. A viable
pair is any two nodes (A,B), regardless of whether they are directly connected,
such that:

 - Node A is not empty (its Used is not zero).
 - Nodes A and B are not the same node.
 - The data on node A (its Used) would fit on node B (its Avail).

How many viable pairs of nodes are there?
-}

import Data.Char(isDigit)
import Data.Map(Map,fromList,member,(!))
import qualified Data.Map

type Node = ((Int,Int),(Int,Int))

parse :: [String] -> Node
parse (name:_:used:available:_) = ((read $ takeWhile isDigit $ dropWhile (not . isDigit) name,read $ dropWhile (not . isDigit) $ dropWhile isDigit $ dropWhile (not . isDigit) name),(read $ takeWhile isDigit used,read $ takeWhile isDigit available))

viablePairs :: [Node] -> [(Node,Node)]
viablePairs nodes = [(a,b) | a@(_,(aused,_)) <- nodes, aused /= 0, b@(_,(_,bavail)) <- nodes, aused <= bavail, a /= b]

test :: ()
test = ()

part1 :: IO Int
part1 = fmap (length . viablePairs . map (parse . words) . drop 2 . lines) $ readFile "input/22.txt"

-- Moving data into an empty node is reversible, so the state needs to
-- be saved to detect loops.
-- Moving data into a non-empty node is irreversible, so do not need
-- to save the state (or any previous states) to avoid loops.

-- There must be some way to avoid the duplication of effort when there
-- are multiple paths (which may or may not be reversible) from one state to
-- another, when the order of steps does not matter.

-- Check the input data to see how many different first steps are possible.
-- If it's severely constrained to the point of of moving one empty node
-- around, the problem won't be too bad.  Maybe there'll be two empty nodes,
-- which might be difficult.  If there are more, I might have to give up.

-- It looks like there is one empty node in my data, and it looks like no
-- other node has enough available space to take anything from any other
-- node.

-- Some nodes can never be in the path:
--  * Those that are too small to hold the goal data.  There are no such
--    nodes in my input data.
--  * Those that have too much data to transfer to any neighboring node.
--    There is a line of large nodes in my input data, all nearly full.  All
--    other nodes, including the empty node, are too small to take data
--    from any of them.
--
-- It also looks like, ignoring the line of the large nodes, no node holds
-- more data than any other node could hold.
--
-- The problem reduces to moving the empty spot next to the node with the
-- goal data, then moving the goal data to node 0,0, with the only constraint
-- of avoiding the line of large nodes.
--
-- So a search is not needed.
--
-- In my input data, the empty node needs to go left to get around the
-- line of large nodes, then to the y=0 row, then to the right to the
-- goal data, then, once the goal data has moved into the empty node,
-- it takes 5 steps to move the goal data left one node.

part2 :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Int
part2 goal@(goalx,goaly) (emptyx,emptyy) large@(largex,largey)
  | emptyx >= largex && emptyy > largey = emptyx - largex + 1 + part2 goal (largex-1,emptyy) large
  | emptyy > 0 = emptyy + part2 goal (emptyx,0) large
  | emptyx + 1 <= goalx = goalx - emptyx - 1 + 1 + 5*(goalx - 1)
  | otherwise = error "?"

test2 :: ()
test2
  | part2 (2,0) (1,1) (0,2) /= 7 = error "a"
  | otherwise = ()

-- Also, since only one node in my input data can receive data at any given
-- time the possibility of simultaneously transferring between multiple pairs
-- of nodes is moot.
