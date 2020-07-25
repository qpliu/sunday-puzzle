data Bounce = Circle Double Double Double Int Outcome
            | Wall Double Double Int Outcome
            | Done Int Outcome
  deriving Show

data Outcome = Unknown | ToLeft | ToRight deriving (Show,Eq)

shoot :: Double -> [Bounce]
shoot targetx
    | discriminant < 0 || t < 0 = bounce (Wall (-targetx) theta0 1 Unknown)
    | otherwise = bounce (Circle x y theta 1 Unknown)
  where
    theta0 = atan2 (2 - targetx) 2
    b = -4*sin theta0
    c = 3
    discriminant = b*b-4*c
    t = -b/2 - sqrt (discriminant/4)
    x = -2 + t*sin theta0
    y = t*cos theta0
    alpha = atan2 y (abs x)
    theta = 2*alpha - theta0

bounce :: Bounce -> [Bounce]
bounce lastBounce@(Circle x0 y0 theta0 n outcome0)
    | cos theta0 <= 0 = [lastBounce,Done n outcome]
    | otherwise = lastBounce:bounce (Wall x theta0 (n+1) outcome)
  where
    t = (2 - y0)/cos theta0
    x = x0 + t*sin theta0
    outcome | outcome0 /= Unknown = outcome0
            | x >= 0 = ToRight
            | otherwise = outcome0
bounce lastBounce@(Wall x0 theta0 n outcome0)
    | discriminant < 0 = [lastBounce,Done n outcome]
    | otherwise = lastBounce:bounce (Circle x y theta (n+1) outcome)
  where
    outcome | outcome0 /= Unknown = outcome0
            | x0 >= 0 = ToRight
            | theta0 < 0 = ToLeft
            | otherwise = outcome0
    b = 2*x0*sin theta0 - 4*cos theta0
    c = x0*x0 + 3
    discriminant = b*b - 4*c
    t = -b/2 - sqrt(discriminant/4)
    x = x0 + t*sin theta0
    y = 2 - t*cos theta0
    alpha = atan2 y (abs x)
    theta | x < 0 = 2*alpha + theta0 - pi
          | otherwise = pi - 2*alpha + theta0
bounces _ = []

s :: Int -> Double -> (Maybe Int,Outcome)
s maxBounces target = o (last (take maxBounces (shoot target)))
  where
    o (Circle _ _ _ _ outcome) = (Nothing,outcome)
    o (Wall _ _ _ outcome) = (Nothing,outcome)
    o (Done n outcome) = (Just n,outcome)

bisect :: Int -> Int -> Double -> Double -> Double
bisect maxBounces maxIterations x1 x2 =
    iter 1 (x1,snd (s maxBounces x1)) (x2,snd (s maxBounces x2))
  where
    iter n (x1,o1) (x2,o2)
      | n >= maxIterations || o1 == o2 = x1
      | o1 == o3 = iter (n+1) (x3,o3) (x2,o2)
      | otherwise = iter (n+1) (x1,o1) (x3,o3)
      where
        x3 = (x1 + x2)/2
        o3 = snd (s maxBounces x3)

main :: IO ()
main = do
    print (bisect 100 200 0.75 0.9)
    print (bisect 100 200 (-1.5) (-2))
