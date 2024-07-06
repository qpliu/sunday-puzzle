al :: Double -> Double
al a = sqrt(a^2 + (beta a)^2)

bl :: Double -> Double
bl b = sqrt((alpha b)^2 + b^2)

alpha :: Double -> Double
alpha b = (-2*b^3 + 6*b^2 - 3*b - sqrt(4*b^6 - 24*b^5 + 57*b^4 - 66*b^3 + 34*b^2))/(3*b - 5)

beta :: Double -> Double
beta a = (-2*a^2 + a + sqrt(a^2*(4*a^2-4*a+17)))/4
