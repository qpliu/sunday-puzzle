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
