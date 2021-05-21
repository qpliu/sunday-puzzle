type Point = (Int,Int)

isIso :: Point -> Point -> Point -> Bool
isIso (a,b) (c,d) (e,f) =
    (a-c)^2 + (b-d)^2 == (a-e)^2 + (b-f)^2
    || (a-c)^2 + (b-d)^2 == (c-e)^2 + (d-f)^2
    || (c-e)^2 + (d-f)^2 == (a-e)^2 + (b-f)^2

isIsoSet :: [Point] -> Bool
isIsoSet [] = False
isIsoSet [_,_] = False
isIsoSet (p:ps) = isIsoSet ps || isIsoSet1 p ps

isIsoSet1 :: Point -> [Point] -> Bool
isIsoSet1 p1 [] = False
isIsoSet1 p1 [_] = False
isIsoSet1 p1 (p:ps) = isIsoSet1 p1 ps || isIsoSet2 p1 p ps

isIsoSet2 :: Point -> Point -> [Point] -> Bool
isIsoSet2 p1 p2 [] = False
isIsoSet2 p1 p2 (p:ps) = isIsoSet2 p1 p2 ps || isIso p1 p2 p

nonisoSetsFrom :: [Point] -> [[Point]]
nonisoSetsFrom [] = [[]]
nonisoSetsFrom (p:ps) = map (p:) (filter (not . isIsoSet1 p) nis) ++ nis
  where nis = nonisoSetsFrom ps

main :: IO ()
main = mapM_ print [(n,maximum (map length (nonisoSetsFrom [(x,y) | x <- [1..n], y <- [1..n]]))) | n <- [1..6]]
