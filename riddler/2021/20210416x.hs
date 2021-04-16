import Data.Set(fromList,size)

type Card = (Int,Int,Int)

rotate :: Card -> Card
rotate (a,b,c) = (b,c,a)

noRepeats :: Card -> Bool
noRepeats (a,b,c) = a /= b && b /= c && c /= a

canonical :: Card -> Card
canonical card = minimum [card, rotate card, rotate (rotate card)]

all1000 :: [Card]
all1000 = [(a,b,c) | a <- [0..9], b <- [0..9], c <- [0..9]]

main :: IO ()
main = do
    (print . size . fromList . filter noRepeats . map canonical) all1000
    (print . size . fromList . map canonical) all1000
