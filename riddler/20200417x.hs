occupied :: (Int,Int) -> Bool
occupied (row,column) = row == 2 || row == 7 || (row == 1 && column /= 4 && column /= 2 && column /= 7) || (row == 8 && column /= 2 && column /= 7)

valid :: (Int,Int) -> Bool
valid (row,column) = not (occupied (row,column)) && row >= 1 && row <= 8 && column >= 1 && column <= 8

moves :: (Int,Int) -> [(Int,Int)]
moves (row,column) = filter valid [(row+1,column+2),(row+2,column+1),(row-1,column+2),(row-2,column+1),(row+1,column-2),(row+2,column-1),(row-1,column-2),(row-2,column-1)]

nextPaths :: [(Int,Int)] -> [[(Int,Int)]]
nextPaths path = map (:path) (moves (head path))

nextTree :: [[(Int,Int)]] -> [[(Int,Int)]]
nextTree paths = concatMap nextPaths paths

fourMoveTree :: [[(Int,Int)]] -> [[(Int,Int)]]
fourMoveTree = nextTree . nextTree . nextTree . nextTree

joinTrees :: [[(Int,Int)]] -> [[(Int,Int)]] -> [[(Int,Int)]]
joinTrees fromStart fromEnd = [reverse s ++ tail e | s <- fromStart, e <- fromEnd, head s == head e]

rightKnightPaths :: [[(Int,Int)]]
rightKnightPaths = joinTrees (fourMoveTree [[(8,7)]]) (fourMoveTree [[(1,4)]])

leftKnightPaths :: [[(Int,Int)]]
leftKnightPaths = joinTrees (nextTree $ nextTree $ nextTree [[(8,2)]]) (nextTree $ nextTree [[(8,7)]])
