p2  (d1,d2,d3,d4,d5) = d1^2
p3  (d1,d2,d3,d4,d5) = 2*d1*d2
p4  (d1,d2,d3,d4,d5) = d2^2 + 2*d1*d3
p5  (d1,d2,d3,d4,d5) = 2*d1*d4 + 2*d2*d3
p6  (d1,d2,d3,d4,d5) = d3^2 + 2*d1*d5 + 2*d2*d4
p7  (d1,d2,d3,d4,d5) = 2*d1*d6 + 2*d2*d5 + 2*d3*d4 where d6 = 1-d1-d2-d3-d4-d5
p8  (d1,d2,d3,d4,d5) = d4^2 + 2*d6*d2 + 2*d5*d3 where d6 = 1-d1-d2-d3-d4-d5
p9  (d1,d2,d3,d4,d5) = 2*d6*d3 + 2*d5*d4 where d6 = 1-d1-d2-d3-d4-d5
p10 (d1,d2,d3,d4,d5) = d5^2 + 2*d6*d4 where d6 = 1-d1-d2-d3-d4-d5
p11 (d1,d2,d3,d4,d5) = 2*d6*d5 where d6 = 1-d1-d2-d3-d4-d5
p12 (d1,d2,d3,d4,d5) = d6^2 where d6 = 1-d1-d2-d3-d4-d5

v d = 1/11*((p2 d - 1/11)^2
          + (p3 d - 1/11)^2
          + (p4 d - 1/11)^2
          + (p5 d - 1/11)^2
          + (p6 d - 1/11)^2
          + (p7 d - 1/11)^2
          + (p8 d - 1/11)^2
          + (p9 d - 1/11)^2
          + (p10 d - 1/11)^2
          + (p11 d - 1/11)^2
          + (p12 d - 1/11)^2)

dp2dd1  (d1,d2,d3,d4,d5) = 2*d1
dp2dd2  (d1,d2,d3,d4,d5) = 0
dp2dd3  (d1,d2,d3,d4,d5) = 0
dp2dd4  (d1,d2,d3,d4,d5) = 0
dp2dd5  (d1,d2,d3,d4,d5) = 0

dp3dd1  (d1,d2,d3,d4,d5) = 2*d2
dp3dd2  (d1,d2,d3,d4,d5) = 2*d1
dp3dd3  (d1,d2,d3,d4,d5) = 0
dp3dd4  (d1,d2,d3,d4,d5) = 0
dp3dd5  (d1,d2,d3,d4,d5) = 0

dp4dd1  (d1,d2,d3,d4,d5) = 2*d3
dp4dd2  (d1,d2,d3,d4,d5) = 2*d2
dp4dd3  (d1,d2,d3,d4,d5) = 2*d1
dp4dd4  (d1,d2,d3,d4,d5) = 0
dp4dd5  (d1,d2,d3,d4,d5) = 0

dp5dd1  (d1,d2,d3,d4,d5) = 2*d4
dp5dd2  (d1,d2,d3,d4,d5) = 2*d3
dp5dd3  (d1,d2,d3,d4,d5) = 2*d2
dp5dd4  (d1,d2,d3,d4,d5) = 2*d1
dp5dd5  (d1,d2,d3,d4,d5) = 0

dp6dd1  (d1,d2,d3,d4,d5) = 2*d5
dp6dd2  (d1,d2,d3,d4,d5) = 2*d4
dp6dd3  (d1,d2,d3,d4,d5) = 2*d3
dp6dd4  (d1,d2,d3,d4,d5) = 2*d2
dp6dd5  (d1,d2,d3,d4,d5) = 2*d1

dp7dd1  (d1,d2,d3,d4,d5) = 2*d6 - 2*d1 where d6 = 1-d1-d2-d3-d4-d5
dp7dd2  (d1,d2,d3,d4,d5) = 2*d5 - 2*d1
dp7dd3  (d1,d2,d3,d4,d5) = 2*d4 - 2*d1
dp7dd4  (d1,d2,d3,d4,d5) = 2*d3 - 2*d1
dp7dd5  (d1,d2,d3,d4,d5) = 2*d2 - 2*d1

dp8dd1  (d1,d2,d3,d4,d5) = 0    - 2*d2
dp8dd2  (d1,d2,d3,d4,d5) = 2*d6 - 2*d2 where d6 = 1-d1-d2-d3-d4-d5
dp8dd3  (d1,d2,d3,d4,d5) = 2*d5 - 2*d2
dp8dd4  (d1,d2,d3,d4,d5) = 2*d4 - 2*d2
dp8dd5  (d1,d2,d3,d4,d5) = 2*d3 - 2*d2

dp9dd1  (d1,d2,d3,d4,d5) = 0    - 2*d3
dp9dd2  (d1,d2,d3,d4,d5) = 0    - 2*d3
dp9dd3  (d1,d2,d3,d4,d5) = 2*d6 - 2*d3 where d6 = 1-d1-d2-d3-d4-d5
dp9dd4  (d1,d2,d3,d4,d5) = 2*d5 - 2*d3
dp9dd5  (d1,d2,d3,d4,d5) = 2*d4 - 2*d3

dp10dd1 (d1,d2,d3,d4,d5) = 0    - 2*d4
dp10dd2 (d1,d2,d3,d4,d5) = 0    - 2*d4
dp10dd3 (d1,d2,d3,d4,d5) = 0    - 2*d4
dp10dd4 (d1,d2,d3,d4,d5) = 2*d6 - 2*d4 where d6 = 1-d1-d2-d3-d4-d5
dp10dd5 (d1,d2,d3,d4,d5) = 2*d5 - 2*d4

dp11dd1 (d1,d2,d3,d4,d5) = 0    - 2*d5
dp11dd2 (d1,d2,d3,d4,d5) = 0    - 2*d5
dp11dd3 (d1,d2,d3,d4,d5) = 0    - 2*d5
dp11dd4 (d1,d2,d3,d4,d5) = 0    - 2*d5
dp11dd5 (d1,d2,d3,d4,d5) = 2*d6 - 2*d5 where d6 = 1-d1-d2-d3-d4-d5

dp12dd1 (d1,d2,d3,d4,d5) = 0    - 2*d6 where d6 = 1-d1-d2-d3-d4-d5
dp12dd2 (d1,d2,d3,d4,d5) = 0    - 2*d6 where d6 = 1-d1-d2-d3-d4-d5
dp12dd3 (d1,d2,d3,d4,d5) = 0    - 2*d6 where d6 = 1-d1-d2-d3-d4-d5
dp12dd4 (d1,d2,d3,d4,d5) = 0    - 2*d6 where d6 = 1-d1-d2-d3-d4-d5
dp12dd5 (d1,d2,d3,d4,d5) = 0    - 2*d6 where d6 = 1-d1-d2-d3-d4-d5

dvdd1 d = 1/11*(2*(p2 d - 1/11)*dp2dd1 d
              + 2*(p3 d - 1/11)*dp3dd1 d
              + 2*(p4 d - 1/11)*dp4dd1 d
              + 2*(p5 d - 1/11)*dp5dd1 d
              + 2*(p6 d - 1/11)*dp6dd1 d
              + 2*(p7 d - 1/11)*dp7dd1 d
              + 2*(p8 d - 1/11)*dp8dd1 d
              + 2*(p9 d - 1/11)*dp9dd1 d
              + 2*(p10 d - 1/11)*dp10dd1 d
              + 2*(p11 d - 1/11)*dp11dd1 d
              + 2*(p12 d - 1/11)*dp12dd1 d)

dvdd2 d = 1/11*(2*(p2 d - 1/11)*dp2dd2 d
              + 2*(p3 d - 1/11)*dp3dd2 d
              + 2*(p4 d - 1/11)*dp4dd2 d
              + 2*(p5 d - 1/11)*dp5dd2 d
              + 2*(p6 d - 1/11)*dp6dd2 d
              + 2*(p7 d - 1/11)*dp7dd2 d
              + 2*(p8 d - 1/11)*dp8dd2 d
              + 2*(p9 d - 1/11)*dp9dd2 d
              + 2*(p10 d - 1/11)*dp10dd2 d
              + 2*(p11 d - 1/11)*dp11dd2 d
              + 2*(p12 d - 1/11)*dp12dd2 d)

dvdd3 d = 1/11*(2*(p2 d - 1/11)*dp2dd3 d
              + 2*(p3 d - 1/11)*dp3dd3 d
              + 2*(p4 d - 1/11)*dp4dd3 d
              + 2*(p5 d - 1/11)*dp5dd3 d
              + 2*(p6 d - 1/11)*dp6dd3 d
              + 2*(p7 d - 1/11)*dp7dd3 d
              + 2*(p8 d - 1/11)*dp8dd3 d
              + 2*(p9 d - 1/11)*dp9dd3 d
              + 2*(p10 d - 1/11)*dp10dd3 d
              + 2*(p11 d - 1/11)*dp11dd3 d
              + 2*(p12 d - 1/11)*dp12dd3 d)

dvdd4 d = 1/11*(2*(p2 d - 1/11)*dp2dd4 d
              + 2*(p3 d - 1/11)*dp3dd4 d
              + 2*(p4 d - 1/11)*dp4dd4 d
              + 2*(p5 d - 1/11)*dp5dd4 d
              + 2*(p6 d - 1/11)*dp6dd4 d
              + 2*(p7 d - 1/11)*dp7dd4 d
              + 2*(p8 d - 1/11)*dp8dd4 d
              + 2*(p9 d - 1/11)*dp9dd4 d
              + 2*(p10 d - 1/11)*dp10dd4 d
              + 2*(p11 d - 1/11)*dp11dd4 d
              + 2*(p12 d - 1/11)*dp12dd4 d)

dvdd5 d = 1/11*(2*(p2 d - 1/11)*dp2dd5 d
              + 2*(p3 d - 1/11)*dp3dd5 d
              + 2*(p4 d - 1/11)*dp4dd5 d
              + 2*(p5 d - 1/11)*dp5dd5 d
              + 2*(p6 d - 1/11)*dp6dd5 d
              + 2*(p7 d - 1/11)*dp7dd5 d
              + 2*(p8 d - 1/11)*dp8dd5 d
              + 2*(p9 d - 1/11)*dp9dd5 d
              + 2*(p10 d - 1/11)*dp10dd5 d
              + 2*(p11 d - 1/11)*dp11dd5 d
              + 2*(p12 d - 1/11)*dp12dd5 d)

step limit gamma d@(d1,d2,d3,d4,d5)
  | limit <= 0 = d
  | v nextd > v d = step (limit-1) (gamma/2) d
  | otherwise = nextd
  where
    nextd = (d1-gamma*dvdd1 d,d2-gamma*dvdd2 d,d3-gamma*dvdd3 d,d4-gamma*dvdd4 d,d5-gamma*dvdd5 d)

d6 (d1,d2,d3,d4,d5) = (d1,d2,d3,d4,d5,1-d1-d2-d3-d4-d5)

main = do
  let d = (1/6,1/6,1/6,1/6,1/6)
  print ("Initial",v d,d6 d)
  let d1000 = last $ take 1000 $ iterate (step 20 0.1) d
  print ("1000 steps",v d1000,d6 d1000)
  let d2000 = last $ take 1000 $ iterate (step 20 0.1) d1000
  print ("2000 steps",v d2000,d6 d2000)
  let d5000 = last $ take 3000 $ iterate (step 20 0.1) d2000
  print ("5000 steps",v d5000,d6 d5000)
  let d10000 = last $ take 5000 $ iterate (step 20 0.1) d5000
  print ("10000 steps",v d10000,d6 d10000)
