play :: Int -> (Int,[Int]) -> (Int,[Int])
play n (start,players) = (i,take i players++drop (i+1) players)
  where i = (start+n-1) `mod` length players

winner :: Int -> Int -> Int
winner n nplayers = game n (0,[1..nplayers])
  where game _ (_,[player]) = player
        game n (i,players) = game n (play n (i,players))

main :: IO ()
main = do
    print (head [n | n <- [1..], (n-1) `mod` 61 == 18, (n-1) `mod` 60 == 31, (n-1) `mod` 59 == 0])
    print (winner 136232 61)
    print (head [n | n <- [1..], winner n 61 == 1])
