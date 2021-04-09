data Crossing = A | B | C | D | E | F | G | H | I

canGo :: Int -> Crossing -> Crossing -> Bool
canGo c A B = c `mod` 2 == 0
canGo c B A = c `mod` 2 /= 0
canGo c B C = (c `div` 2) `mod` 2 == 0
canGo c C B = (c `div` 2) `mod` 2 /= 0
canGo c A D = (c `div` 4) `mod` 2 == 0
canGo c D A = (c `div` 4) `mod` 2 /= 0
canGo c B E = (c `div` 8) `mod` 2 == 0
canGo c E B = (c `div` 8) `mod` 2 /= 0
canGo c C F = (c `div` 16) `mod` 2 == 0
canGo c F C = (c `div` 16) `mod` 2 /= 0
canGo c D E = (c `div` 32) `mod` 2 == 0
canGo c E D = (c `div` 32) `mod` 2 /= 0
canGo c E F = (c `div` 64) `mod` 2 == 0
canGo c F E = (c `div` 64) `mod` 2 /= 0
canGo c D G = (c `div` 128) `mod` 2 == 0
canGo c G D = (c `div` 128) `mod` 2 /= 0
canGo c E H = (c `div` 256) `mod` 2 == 0
canGo c H E = (c `div` 256) `mod` 2 /= 0
canGo c F I = (c `div` 512) `mod` 2 == 0
canGo c I F = (c `div` 512) `mod` 2 /= 0
canGo c G H = (c `div` 1024) `mod` 2 == 0
canGo c H G = (c `div` 1024) `mod` 2 /= 0
canGo c H I = (c `div` 2048) `mod` 2 == 0
canGo c I H = (c `div` 2048) `mod` 2 /= 0
canGo c _ _ = False

paths :: [[Crossing]]
paths = [
  [A,B,C,F,I],
  [A,B,C,F,E,H,I],
  [A,B,C,F,E,D,G,H,I],
  [A,B,E,F,I],
  [A,B,E,H,I],
  [A,B,E,D,G,H,I],
  [A,D,G,H,I],
  [A,D,G,H,E,F,I],
  [A,D,G,H,E,B,C,F,I],
  [A,D,E,H,I],
  [A,D,E,F,I],
  [A,D,E,B,C,F,I]
  ]

canUsePath :: Int -> [Crossing] -> Bool
canUsePath c [] = True
canUsePath c [_] = True
canUsePath c (a:p@(b:_)) = canGo c a b && canUsePath c p

hasPath :: Int -> Bool
hasPath c = any (canUsePath c) paths

main :: IO ()
main = print (length (filter hasPath [0..4095]))
