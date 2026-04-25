import Data.List(sort)

-- Fiddler
-- When the hour hand is on a minute, the minute hand can only
-- be on :00, :12, :24, :36, or :48.  Of these, the only one
-- that can have the hour hand 13 minutes ahead of it is :24,
-- so the answer is 7:24.

-- Extra credit
-- Let 0 < t < 12 be the time in hours.
-- The minute hand is at m = 12(t - floor(t))
-- and the second hand is at s = 12(60t-floor(60t))

-- There are six ways the hands can be ordered.

-- First, consider, when going clockwise, the hands are hour,
-- minute, second.  Then
--     t = m - 3 = s - 6
--  or t = m - 3 = s + 6
--  or t = m + 9 = s + 6
-- and
--  f_1 = min((t-m+3)^2, (t-m-9)^2) + min((m-s+3)^2, (m-s-9)^2)

-- If the hands are hour, second, minute, then
--  f_2 = min((t-s+3)^2, (t-s-9)^2) + min((s-m+3)^2, (s-m-9)^2)

-- If the hands are minute, hour, second, then
--  f_3 = min((m-t+3)^2, (m-t-9)^2) + min((t-s+3)^2, (t-s-9)^2)

-- If the hands are minute, second, hour, then
--  f_4 = min((m-s+3)^2, (m-s-9)^2) + min((s-t+3)^2, (s-t+9)^2)

-- If the hands are second, hour, minute, then
--  f_5 = min((s-t+3)^2, (s-t-9)^2) + min((t-m+3)^2, (t-m+9)^2)

-- If the hands are second, minute, hour, then
--  f_6 = min((s-m+3)^2, (s-m-9)^2) + min((m-t+3)^2, (m-t+9)^2)

-- Then, f = min(f_1, f_2, f_3, f_4, f_5, f_6)

m :: Rational -> Rational
m t = 12*(t - fromIntegral (floor t))

s :: Rational -> Rational
s t = 12*(60*t - fromIntegral (floor (60*t)))

f1 :: Rational -> Rational
f1 t = min ((t - m t + 3)^2) ((t - m t - 9)^2) + min ((m t - s t + 3)^2) ((m t - s t - 9)^2)

f2 :: Rational -> Rational
f2 t = min ((t - s t + 3)^2) ((t - s t - 9)^2) + min ((s t - m t + 3)^2) ((s t - m t - 9)^2)

f3 :: Rational -> Rational
f3 t = min ((m t - t + 3)^2) ((m t - t - 9)^2) + min ((t - s t + 3)^2) ((t - s t - 9)^2)

f4 :: Rational -> Rational
f4 t = min ((m t - s t + 3)^2) ((m t - s t - 9)^2) + min ((s t - t + 3)^2) ((s t - t - 9)^2)

f5 :: Rational -> Rational
f5 t = min ((s t - t + 3)^2) ((s t - t - 9)^2) + min ((t - m t + 3)^2) ((t - m t - 9)^2)

f6 :: Rational -> Rational
f6 t = min ((s t - m t + 3)^2) ((s t - m t - 9)^2) + min ((m t - t + 3)^2) ((m t - t - 9)^2)

f :: Rational -> Rational
f t = minimum [f1 t, f2 t, f3 t, f4 t, f5 t, f6 t]

time :: Rational -> String
time t = show (floor t) ++ (if 5*m t < 10 then ":0" else ":") ++ show (floor (5*m t)) ++ (if 5*s t < 10 then ":0" else ":") ++ show (floor (5*s t)) ++ "." ++ show (5*s t - fromIntegral (floor (5*s t)))

search :: Int -> Int -> Rational -> Rational -> [(Rational,Double,Rational,String)]
search nbest nt t0 dt = take nbest $ sort [(f t,fromRational (f t),t,time t) | t <- [t0-trange,t0-trange+dt .. t0+trange]]
  where trange = fromIntegral nt*dt

main :: IO ()
main = do
    print $ search 10 (3600*6) 6 (1/3600)
    print $ search 10 1000 (1166/225) (1/3600000)
    print $ search 10 1000 (1534/225) (1/3600000)
    print $ search 10 1000 (859/225) (1/3600000)
    print $ search 10 1000 (1841/225) (1/3600000)
    print $ search 10 1000 (307/225) (1/3600000)
    print $ search 10 1000 (1043/225) (1/3600000)

-- Minimum f for times at whole milliseconds:
-- 3:49:04.079
-- 8:10:55.921

-- The exact times are
-- 3887379/1018255, or 3:49:04 3200/40729
-- 8331321/1018225, or 8:10:55 37529/40729
