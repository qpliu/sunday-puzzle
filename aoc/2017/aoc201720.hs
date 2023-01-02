{-
--- Day 20: Particle Swarm ---

Suddenly, the GPU contacts you, asking for help. Someone has asked it to
simulate too many particles, and it won't be able to finish them all in time to
render the next frame at this rate.

It transmits to you a buffer (your puzzle input) listing each particle in order
(starting with particle 0, then particle 1, particle 2, and so on). For each
particle, it provides the X, Y, and Z coordinates for the particle's position
(p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.

Each tick, all particles are updated simultaneously. A particle's properties
are updated in the following order:

 - Increase the X velocity by the X acceleration.
 - Increase the Y velocity by the Y acceleration.
 - Increase the Z velocity by the Z acceleration.
 - Increase the X position by the X velocity.
 - Increase the Y position by the Y velocity.
 - Increase the Z position by the Z velocity.

Because of seemingly tenuous rationale involving z-buffering, the GPU would
like to know which particle will stay closest to position <0,0,0> in the long
term. Measure this using the Manhattan distance, which in this situation is
simply the sum of the absolute values of a particle's X, Y, and Z position.

For example, suppose you are only given two particles, both of which stay
entirely on the X-axis (for simplicity). Drawing the current states of
particles 0 and 1 (in that order) with an adjacent a number line and diagram of
current X positions (marked in parentheses), the following would take place:

| p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
| p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)
| 
| p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
| p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)
| 
| p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
| p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)
| 
| p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
| p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)   

At this point, particle 1 will never be closer to <0,0,0> than particle 0, and
so, in the long run, particle 0 will stay closest.

Which particle will stay closest to position <0,0,0> in the long term?
-}

import Data.List(delete)
import Data.Map(Map,empty,alter,member)
import qualified Data.Map

type XYZ = (Int,Int,Int)
type Particle = (XYZ,XYZ,XYZ)

parse :: String -> [(Int,Particle)]
parse = parse1 0
  where
    parse1 n s
      | null az = []
      | otherwise = (n,((read px,read py,read pz),(read vx,read vy,read vz),(read ax,read ay,read az))) : parse1 (n+1) s12
      where
        s1 = dropWhile (/= '<') s
        (px,s2) = span (/= ',') $ dropWhile (== ' ') $ drop 1 s1
        (py,s3) = span (/= ',') $ dropWhile (== ' ') $ drop 1 s2
        (pz,s4) = span (/= '>') $ dropWhile (== ' ') $ drop 1 s3
        s5 = dropWhile (/= '<') s4
        (vx,s6) = span (/= ',') $ dropWhile (== ' ') $ drop 1 s5
        (vy,s7) = span (/= ',') $ dropWhile (== ' ') $ drop 1 s6
        (vz,s8) = span (/= '>') $ dropWhile (== ' ') $ drop 1 s7
        s9 = dropWhile (/= '<') s8
        (ax,s10) = span (/= ',') $ dropWhile (== ' ') $ drop 1 s9
        (ay,s11) = span (/= ',') $ dropWhile (== ' ') $ drop 1 s10
        (az,s12) = span (/= '>') $ dropWhile (== ' ') $ drop 1 s11

longterm :: (Int,Particle) -> ((Int,Int,Int),Int)
longterm (particle,((px,py,pz),(vx,vy,vz),(ax,ay,az))) = ((abs ax + abs ay + abs az,(if ax == 0 then abs vx else 0)+(if ay == 0 then abs vy else 0)+(if az == 0 then abs vz else 0),(if ax == 0 && vx == 0 then abs px else 0)+(if ay == 0 && vy == 0 then abs py else 0)+(if az == 0 && vz == 0 then abs pz else 0)),particle)

closest :: [(Int,Particle)] -> Int
closest = snd . minimum . map longterm

testData :: String
testData = "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>\np=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"

test :: ()
test
  | closest (parse testData) /= 0 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (closest . parse) $ readFile "input/20.txt"

-- My input data has 1000 particles.

-- After all particles that have collided are removed,
-- if a particle has the greatest positive x acceleration, the greatest
-- positive x velocity and the greatest x, it can be counted as remaining
-- and removed from further calculation.  Same for the particle with most
-- negative acceleration, the most negative x velocity, and the least x.
-- Same for y and z.

-- This can be repeated until there are no such particles.

tick :: Particle -> Particle
tick ((px,py,pz),(vx,vy,vz),(ax,ay,az)) = ((px+vx+ax,py+vy+ay,pz+vz+az),(vx+ax,vy+ay,vz+az),(ax,ay,az))

removeCollisions :: [(Int,Particle)] -> [(Int,Particle)]
removeCollisions list = filter survives list
  where
    collided = Data.Map.filter (> 1) (foldl collect empty list)
    collect m (_,(p,_,_)) = alter (Just . maybe 1 (+1)) p m
    survives (_,(p,_,_)) = not (p `member` collided)

testData2 :: String
testData2 = "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>\np=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>\np=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>\np=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"

findExtreme :: (Particle -> Int) -> (Particle -> Int) -> (Particle -> Int) -> (Int -> Int -> Int) -> [(Int,Particle)] -> Maybe (Int,Particle)
findExtreme getP getV getA getExtreme [] = Nothing
findExtreme getP getV getA getExtreme (item@(_,first):rest) = toResult $ foldr consider (item,getP first,getV first,getA first) rest
  where
    toResult (item@(_,particle),p,v,a)
      | getP particle == p && getV particle == v && getA particle == a = Just item
      | otherwise = Nothing
    consider item@(_,particle) (olditem,p,v,a)
      | getP particle == newp && getV particle == newv && getA particle == newa = (item,newp,newv,newa)
      | otherwise = (olditem,newp,newv,newa)
      where
        newp = getExtreme p (getP particle)
        newv = getExtreme v (getV particle)
        newa = getExtreme a (getA particle)

getPX :: Particle -> Int
getPX ((px,_,_),_,_) = px

getPY :: Particle -> Int
getPY ((_,py,_),_,_) = py

getPZ :: Particle -> Int
getPZ ((_,_,pz),_,_) = pz

getVX :: Particle -> Int
getVX (_,(vx,_,_),_) = vx

getVY :: Particle -> Int
getVY (_,(_,vy,_),_) = vy

getVZ :: Particle -> Int
getVZ (_,(_,_,vz),_) = vz

getAX :: Particle -> Int
getAX (_,_,(ax,_,_)) = ax

getAY :: Particle -> Int
getAY (_,_,(_,ay,_)) = ay

getAZ :: Particle -> Int
getAZ (_,_,(_,_,az)) = az

removeExtremes :: (Int,[(Int,Particle)]) -> (Int,[(Int,Particle)])
removeExtremes state@(removeCount,list) = rmMaxX
  where
    rmMaxX = maybe rmMinX recurse $ findExtreme getPX getVX getAX max list
    rmMinX = maybe rmMaxY recurse $ findExtreme getPX getVX getAX min list
    rmMaxY = maybe rmMinY recurse $ findExtreme getPY getVY getAY max list
    rmMinY = maybe rmMaxZ recurse $ findExtreme getPY getVY getAY min list
    rmMaxZ = maybe rmMinZ recurse $ findExtreme getPZ getVZ getAZ max list
    rmMinZ = maybe state  recurse $ findExtreme getPZ getVZ getAZ min list
    recurse extreme = removeExtremes (removeCount+1,delete extreme list)

part2 :: IO Int
part2 = fmap (fst . head . dropWhile ((> 0) . length . snd) . iterate (fmap (map (fmap tick)) . removeExtremes . fmap removeCollisions) . ((,) 0) . parse) $ readFile "input/20.txt"
