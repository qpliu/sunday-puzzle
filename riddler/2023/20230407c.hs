import Data.List(sort)

p :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational
p g sy sn ayy ayn any ann =
    g*sy*ayy*allYes (sort [g,sy,ayy])
    + g*sy*(1-ayy)*oneNo ayy (sort [g,sy])
    + g*(1-sy)*ayn*oneNo sy (sort [g,ayn])
    + g*(1-sy)*(1-ayn)*oneYes g (sort [sy,ayn])
    + (1-g)*sn*any*oneNo g (sort [sn,any])
    + (1-g)*sn*(1-any)*oneYes sn (sort [g,any])
    + (1-g)*(1-sn)*ann*oneYes ann (sort [g,sn])
    + (1-g)*(1-sn)*(1-ann)*allNo (sort [g,sn,ann])

allYes :: [Rational] -> Rational
allYes [z,y,x] = 1 - y/(2*x) - z^2/(6*x*y)

oneYes :: Rational -> [Rational] -> Rational
oneYes a [y,x]
  | a > x = (1-x^3)/(3*a*(1-x)*(1-y)) - (1+x)*y/(2*a*(1-y))
  | otherwise = (1+x)/(2*(1-y)) - y/(1-y)

-- This is wrong
oneNo :: Rational -> [Rational] -> Rational
oneNo a [y,x]
  | a > x = 1
  | a > y = (2*a-3*a^2/2-x+x^2/2)/(x*(1-a))
  | otherwise = a^2/(1-a) + x*y/(1-a) - (x+y)*(1+a)/(2*x*y) + (1-a^3)/(3*x*y*(1-a))

allNo :: [Rational] -> Rational
allNo [z,y,x] = ((1-x^3)/3 - (y+z)*(1-x^2)/2 + y*z*(1-x))/((1-x)*(1-y)*(1-z))
