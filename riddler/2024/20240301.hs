a1c s x y = (10-s)*(10-y) + (x+s-10)*(10-y)/2 + s*y + (x-s)*y/2
a2c s x y = (20-x)*(10-y) + (x+s-10)*(10-y)/2  + (20-x)*(y-s)/2
a3c s x y = (x-s)*y/2 + s*(20-x) + (20-x)*(y-s)/2
asc s x y = a1c s x y + a2c s x y + a3c s x y

a1 s x y = 50 - 5*s + 5*x + (s-5)*y
a2 s x y = 150 - 5*s + (s/2-5)*x - (s/2+5)*y
a3 s x y = 10*s - s*x/2 + (10-s/2)*y
as s x y = a1 s x y + a2 s x y + a3 s x y

x1 s = (2000-300*s+30*s^2)/(200-20*s+2*s^2)

y1 s = (50 + 15*s - 15*(2000-300*s+30*s^2)/(200-20*s+2*s^2))/(3*(s-5))
y2 s = (500 - 30*s + 3*(s-10)*(2000-300*s+30*s^2)/(200-20*s+2*s^2))/(3*(s+10))
y3 s = (400 - 60*s + 3*s*(2000-300*s+30*s^2)/(200-20*s+2*s^2))/(3*(20-s))
