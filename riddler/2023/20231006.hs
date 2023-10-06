l :: [(Integer,(Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer))]
l = [(n,(n8,n7,n6,n5,n4,n3,n2,n1)) | n8 <- [0..8],
         n7 <- [0..7], n7 == 0 || n7 /= n8,
         n6 <- [0..6], n6 == 0 || (n6 /= n8 && n6 /= n7),
         n5 <- [0..5], n5 == 0 || (n5 /= n8 && n5 /= n7 && n5 /= n6),
         n4 <- [0..4], n4 == 0 || (n4 /= n8 && n4 /= n7 && n4 /= n6 && n4 /= n5),
         n3 <- [0..3], n3 == 0 || (n3 /= n8 && n3 /= n7 && n3 /= n6 && n3 /= n5 && n3 /= n4),
         n2 <- [0..2], n2 == 0 || (n2 /= n8 && n2 /= n7 && n2 /= n6 && n2 /= n5 && n2 /= n4 && n2 /= n3),
         n1 <- [0..1], n1 == 0 || (n1 /= n8 && n1 /= n7 && n1 /= n6 && n1 /= n5 && n1 /= n4 && n1 /= n2),
         n <- [n8*product [1..8] + n7*product [1..7] + n6*product [1..6] + n5*product [1..5] + n4*product [1..4] + n3*product [1..3] + n2*product [1..2] + n1],
         n `mod` 2520 == 0]

l2 :: [(Integer,(Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer))]
l2 = [(n,(n8,n7,n6,n5,n4,n3,n2,n1)) | n8 <- [0..8],
         n7 <- [0..7],
         n6 <- [0..6],
         n5 <- [0..5],
         n4 <- [0..4],
         n3 <- [0..3],
         n2 <- [0..2],
         n1 <- [0..1],
         n <- [n8*product [1..8] + n7*product [1..7] + n6*product [1..6] + n5*product [1..5] + n4*product [1..4] + n3*product [1..3] + n2*product [1..2] + n1],
         n `mod` 2520 == 0]
