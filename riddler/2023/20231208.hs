guess :: Eq a => [a] -> Int -> [[a]] -> [[a]]
guess guessSet guessResult possibleSets = filter ((`elem` [guessResult,length guessSet - guessResult]) . length . filter (`elem` guessSet)) possibleSets

allSets :: [String]
allSets = [['A',b,c,d] | b <- ['B'..'H'], c <- [succ b..'H'], d <- [succ c..'H']]

g :: Eq a => [a] -> [[a]] -> ((Int,Int),([[a]],[[a]]))
g guessSet possibleSets = ((length p1,length p2),(p1,p2))
  where
    p1 = guess guessSet 1 possibleSets
    p2 = guess guessSet 2 possibleSets