import Data.List(nub,permutations)
import Data.Map(Map,alter,empty,toList)
import Data.Time(getCurrentTime)

data Hat = R | Y | B deriving (Eq,Ord,Show)

type Strategy = Hat -> Hat -> Hat

applyEW :: Strategy -> Strategy -> [(Hat,Hat,Hat,Hat)]
applyEW es ws = [(n,e,s,w) | n <- [R,Y,B], e <- [R,Y,B], s <- [R,Y,B], w <- [R,Y,B], e /= es n s && w /= ws n s]

gatherNS :: [(Hat,Hat,Hat,Hat)] -> Map (Hat,Hat) [(Hat,Hat)]
gatherNS = foldl collect empty
  where
    collect m (n,e,s,w) = alter (Just . maybe [(n,s)] ((n,s):)) (e,w) m

guessable :: [(Hat,Hat)] -> Bool
guessable cases = or [all (ok a b) cases | a <- [R,Y,B], b <- [R,Y,B]]
  where
    ok a b (c,d) = a == c || b == d

s :: [Hat] -> Strategy
s [rr,ry,rb,yr,yy,yb,br,by,bb] R R = rr
s [rr,ry,rb,yr,yy,yb,br,by,bb] R Y = ry
s [rr,ry,rb,yr,yy,yb,br,by,bb] R B = rb
s [rr,ry,rb,yr,yy,yb,br,by,bb] Y R = yr
s [rr,ry,rb,yr,yy,yb,br,by,bb] Y Y = yy
s [rr,ry,rb,yr,yy,yb,br,by,bb] Y B = yb
s [rr,ry,rb,yr,yy,yb,br,by,bb] B R = br
s [rr,ry,rb,yr,yy,yb,br,by,bb] B Y = by
s [rr,ry,rb,yr,yy,yb,br,by,bb] B B = bb

test :: Strategy -> Strategy -> [((Hat,Hat),[(Hat,Hat)])]
test es ws = filter (not . guessable . snd) $ toList $ gatherNS $ applyEW es ws

t :: Strategy -> Strategy -> IO ()
t es ws = mapM_ print $ test es ws

win :: Strategy -> Strategy -> Bool
win es ws = all (guessable . snd) $ toList $ gatherNS $ applyEW es ws

search :: Int -> Int -> [([Hat],[Hat])] -> IO ()
search blockSize n [] = print "done"
search blockSize n strategies = do
    getCurrentTime >>= print . ((,) n)
    mapM_ print $ searchBlock (take blockSize strategies)
    search blockSize (n+blockSize) (drop blockSize strategies)
  where
    searchBlock [] = []
    searchBlock ((es,ws):rest)
      | win (s es) (s ws) = (es,ws) : searchBlock rest
      | otherwise = searchBlock rest

main :: IO ()
main = do
    search 100000 0 [(es,ws) | es <- strategies, ws <- strategies]
  where
    strategies = nub $ permutations [R,R,R,Y,Y,Y,B,B,B]

test4 :: Strategy -> Strategy -> Strategy -> Strategy -> [(Hat,Hat,Hat,Hat)]
test4 ns es ss ws = [(n,e,s,w) | n <- [R,Y,B], e <- [R,Y,B], s <- [R,Y,B], w <- [R,Y,B], n /= ns e w && e /= es n s && s /= ss e w && w /= ws n s]
