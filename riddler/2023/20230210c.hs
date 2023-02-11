h = 6/4000

phi theta = pi/2 - theta - atan (sin theta/(h + 1 - cos theta))

alpha theta = sin theta/(h + 1 - cos theta)

dalpha theta = h*cos theta/((h+1-cos theta)^2)

dphi theta = -1 + dalpha theta/(1+(alpha theta)^2)

f theta = 2*acos(1/(1+h))/pi + dphi theta

bisect epsilon f x1 x2
  | abs (f x) < epsilon = x
  | f x1 < 0 && f x2 > 0 && f x < 0 = bisect epsilon f x x2
  | f x1 > 0 && f x2 < 0 && f x > 0 = bisect epsilon f x x2
  | f x1 < 0 && f x2 > 0 && f x > 0 = bisect epsilon f x1 x
  | f x1 > 0 && f x2 < 0 && f x < 0 = bisect epsilon f x1 x
  where x = (x1+x2)/2

main = print (phi (bisect 1e-15 f 0 (acos (1/(1+h)))))
