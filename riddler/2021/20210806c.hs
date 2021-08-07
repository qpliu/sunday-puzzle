data Move = M1 | M2 | M3 | M4 | M5 | M6 | M7 | M8 | M9
  deriving (Bounded,Enum,Eq,Ord,Show)

newtype X = X (Int,Int) deriving Show
newtype V = V (Int,Int) deriving Show

data Quadrant = LR | UR | UL | LL deriving (Enum,Eq)

accel :: V -> Move -> V
accel (V (dx,dy)) M1 = V (dx-1,dy+1)
accel (V (dx,dy)) M2 = V (dx,dy+1)
accel (V (dx,dy)) M3 = V (dx+1,dy+1)
accel (V (dx,dy)) M4 = V (dx-1,dy)
accel (V (dx,dy)) M5 = V (dx,dy)
accel (V (dx,dy)) M6 = V (dx+1,dy)
accel (V (dx,dy)) M7 = V (dx-1,dy-1)
accel (V (dx,dy)) M8 = V (dx,dy-1)
accel (V (dx,dy)) M9 = V (dx+1,dy-1)

crossFinishAt :: X -> V -> Quadrant -> Int -> Maybe Rational
crossFinishAt (X (x,y)) (V (dx,dy)) quad t
  | y >= 0 || x >= 0 || x + dx < 0 || quad /= LL = Nothing
  | otherwise = Just (fromIntegral t - fromIntegral x / fromIntegral dx)

hitOuterwall :: X -> V -> Bool
hitOuterwall (X (x,y)) (V (dx,dy)) = abs (x+dx) > 7 || abs (y+dy) > 7

hitCircle :: X -> V -> Bool
hitCircle (X (x,y)) (V (dx,dy))
  | dx == 0 && abs x >= 3 = False
  | dx == 0 && y*(y+dy) <= 0 = True
  | dx == 0 = x^2 + (min (abs y) (abs (y+dy)))^2 < 9
  | dy == 0 && abs y >= 3 = False
  | dy == 0 && x*(x+dx) <= 0 = True
  | dy == 0 = y^2 + (min (abs x) (abs (x+dx)))^2 < 9
  | x2*x2 + y2*y2 < 9 = True
  | otherwise = xmin*xmin + ymin*ymin < 9 && (x1 < xmin && xmin < x2) || (x1 > xmin && xmin > x2)
  where
    x1 = fromIntegral x :: Rational
    x2 = fromIntegral (x+dx)
    y1 = fromIntegral y
    y2 = fromIntegral (y+dy)
    xmin = (y2^2*x1 + y1*y2*(x1+x2) + y1^2*x2)/((x1-x2)^2 + (y1-y2)^2)
    ymin = (y2*(xmin-x1) - y1*(xmin-x2))/(x2-x1)

hitWall :: X -> V -> Bool
hitWall x v = hitOuterwall x v || hitCircle x v

move :: X -> V -> X
move (X (x,y)) (V (dx,dy)) = X (x+dx,y+dy)

nextQuad :: X -> Quadrant -> Quadrant
nextQuad (X (x,y)) quad
  | quad == LR && x > 0 && y > 0 = UR
  | quad == UR && x < 0 && y > 0 = UL
  | quad == UL && x < 0 && y < 0 = LL
  | otherwise = quad

searchFrom :: [Move] -> X -> V -> Quadrant -> Int -> [(Rational,[Move])]
searchFrom moves x v quad t =
    maybe keepSearching ((:[]) . flip (,) moves) (crossFinishAt x v quad t)
  where
    keepSearching
      | hitWall x v = []
      | quad == LR && t > 6 = []
      | quad == UR && t > 9 = []
      | quad == UL && t > 12 = []
      | t > 12 = []
      | null searchResults = []
      | otherwise = [minimum searchResults]
    searchResults = concat [searchFrom (m:moves) (move x v) (accel v m) (nextQuad x quad) (t+1) | m <- [minBound..maxBound]]

steps :: X -> V -> [Move] -> [(X,V)]
steps x v [] = [(x,v)]
steps x v (m:moves) = (x,v) : steps (move x (accel v m)) (accel v m) moves

main :: IO ()
main = do
  let [(t,moves)] = searchFrom [] (X (0,-5)) (V (0,0)) LR 0
  print (t-1) -- the initial move (X (0,-5)) (V (0,0)) counts as one move in the code, so subtract one
  print (reverse moves)
