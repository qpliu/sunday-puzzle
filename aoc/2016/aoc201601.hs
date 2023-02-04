import Data.Char(isDigit)
import Data.Set(Set,empty,insert,member)

follow1 :: ((Int,Int),(Int,Int)) -> String -> ((Int,Int),(Int,Int))
follow1 ((dx,dy),(x,y)) (turn:steps) = ((newdx,newdy),(x+n*newdx,y+n*newdy))
  where
    n = read $ takeWhile isDigit steps
    (newdx,newdy) | turn == 'L' = (-dy,dx) | otherwise = (dy,-dx)

follow :: [String] -> (Int,Int)
follow insns = snd $ foldl follow1 ((0,1),(0,0)) insns

distance :: (Int,Int) -> Int
distance (x,y) = abs x+abs y

test :: ()
test
  | follow ["R2","L3"] /= (2,3) = error "a"
  | distance (follow ["R2","L3"]) /= 5 = error "b"
  | follow ["R2","R2","R2"] /= (0,-2) = error "c"
  | distance (follow ["R2","R2","R2"]) /= 2 = error "d"
  | distance (follow ["R5","L5","R5","R3"]) /= 12 = error "e"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (distance . follow . words) $ readFile "input/01.txt"

part2follow1 :: (Set (Int,Int),((Int,Int),(Int,Int))) -> String -> Either (Int,Int) (Set (Int,Int),((Int,Int),(Int,Int)))
part2follow1 (visited,((dx,dy),(x,y))) (turn:steps) = f1 visited (x,y) n
  where
    n = read $ takeWhile isDigit steps
    (newdx,newdy) | turn == 'L' = (-dy,dx) | otherwise = (dy,-dx)
    f1 v (newx,newy) i
      | (newx,newy) `member` visited = Left (newx,newy)
      | i <= 0 = Right (v,((newdx,newdy),(newx,newy)))
      | otherwise = f1 (insert (newx,newy) v) (newx+newdx,newy+newdy) (i-1)

part2follow :: [String] -> (Int,Int)
part2follow insns = f insns (empty,((0,1),(0,0)))
  where
    f insns state = either id (f (tail insns)) (part2follow1 state (head insns))

test2 :: ()
test2
  | part2follow (words "R8, R4, R4, R8") /= (4,0) = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (distance . part2follow . words) $ readFile "input/01.txt"
