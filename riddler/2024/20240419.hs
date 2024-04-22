import Data.Map(Map,alter,empty,fromList,member,toList,(!))

p :: Int -> Map (Int,Int) Rational
p maxDeckSize = table
  where
    table = fromList [((n,m),prob n m) | n <- [0..maxDeckSize], m <- [-1..n+2]]
    prob n m
      | n <= 0 || m < 0 = 0
      | m >= n = 0
      | otherwise = (nn-mm)/nn^2
                  + (nn-mm)*(nn-mm-1)/nn^2 * table!(n-1,m+1)
                  + 2*(nn-mm)*mm/nn^2 * table!(n-1,m)
                  + mm^2/nn^2 * table!(n-1,m-1)
      where (nn,mm) = (fromIntegral n,fromIntegral m)

main :: IO ()
main = do
    let w = 1 - p 52!(52,0) in print (w,fromRational w)
    let ec = 1 - sum [ecp 52!(52,52-2*i)*ecdeck 52!(52,i) | i <- [0..26]] in print (ec,fromRational ec)

ecdeck :: Int -> Map (Int,Int) Rational
ecdeck deckSize = table
  where
    table = fromList [((n,p),prob n p) | n <- [0..deckSize], p <- [-1..deckSize]]
    prob n p
      | 2*p > n = 0
      | n == 0 && p == 0 = 1
      | n == 0 = 0
      | p < 0 = 0
      | otherwise = (2*d-2*(nn-1)+2*pp)/(2*d-(nn-1)) * table!(n-1,p)
                  + (nn-1-2*(pp-1))/(2*d-(nn-1)) * table!(n-1,p-1)
      where (d,nn,pp) = (fromIntegral deckSize,fromIntegral n,fromIntegral p)

ecp :: Int -> Map (Int,Int) Rational
ecp deckSize = table
  where
    table = fromList [((n,m),prob n m) | n <- [0..deckSize], m <- [-2..n+1]]
    prob n m
      | m <= 0 = 0
      | n <= 0 = 0
      | m > n = 0
      | otherwise = mm/nn^2
                  + mm*(mm-1)/nn^2 * table!(n-1,m-2)
                  + 2*(nn-mm)*mm/nn^2 * table!(n-1,m-1)
                  + (nn-mm)^2/nn^2 * table!(n-1,m)
      where (nn,mm) = (fromIntegral n,fromIntegral m)

type State = Map (Int,Int) Int

initialState :: Int -> Int -> State
initialState nsuits nranks = fromList $ ((nsuits,nsuits),nranks) : [((a,b),0) | a <- [0..nsuits], b <- [0..nsuits], a /= nsuits || b /= nsuits]

states :: Int -> Int -> [State]
states nsuits nranks = filter (valid nranks) [fromList (zip (rankStates nsuits) rankCounts) | rankCounts <- rankCountsSet ((1+nsuits)^2) nranks]

rankStates :: Int -> [(Int,Int)]
rankStates nsuits = [(a,b) | a <- [0..nsuits], b <- [0..nsuits]]

rankCountsSet :: Int -> Int -> [[Int]]
rankCountsSet remainingSlots remainingRanks
      | remainingSlots <= 0 = []
      | remainingSlots == 1 = [[remainingRanks]]
      | otherwise = concat [map (ranks:) (rankCountsSet (remainingSlots-1) (remainingRanks-ranks)) | ranks <- [0..remainingRanks]]

cardCount :: ((Int,Int) -> Int) -> State -> Int
cardCount chooseDeck state = sum [chooseDeck cardCounts*rankCount | (cardCounts,rankCount) <- toList state]

valid :: Int -> State -> Bool
valid nranks state =
    cardCount fst state == cardCount snd state && sum state == nranks

makeTerminals :: [State] -> Map State Rational
makeTerminals allStates = fromList $ [(s,0) | s <- allStates, isWin s] ++ [(s,1) | s <- allStates, isLose s]
  where
    isWin state = and [a == 0 || b == 0 || n == 0 | ((a,b),n) <- toList state]
    isLose state = length [() | ((a,b),n) <- toList state, a == b, n == 1] == 1 && length [() | ((a,b),n) <- toList state, n > 0] == 0

transitions :: Map State Rational -> State -> (Rational,Map State Rational)
transitions terminals state
  | member state terminals = (terminals!state,empty)
  | otherwise = foldl combine (0,empty) [draw ranka rankb | ranka <- toList state, rankb <- toList state]
  where
    deckSize = fromIntegral $ cardCount fst state
    draw ranka@((ncaa,ncab),nra) rankb@((ncba,ncbb),nrb)
      | nra <= 0 || nrb <= 0 = (0,[])
      | ranka == rankb = (ncaa*nra*ncbb,[(ncaa*nra*ncbb*(nrb-1),fromList [if rank == ranka then ((nca,ncb),nr-2) else if (nca,ncb) == (ncaa-1,ncab) || (nca,ncb) == (ncba,ncbb-1) then ((nca,ncb),nr+1) else rank | rank@((nca,ncb),nr) <- toList state])])
      | otherwise = (0,[(ncaa*nra*ncbb*nrb,fromList [if (nca,ncb) == (ncaa-1,ncab) && (nca,ncb) == (ncba,ncbb-1) then ((nca,ncb),nr+2) else if (nca,ncb) == (ncaa-1,ncab) && (nca,ncb) == (ncba,ncbb) then rank else if (nca,ncb) == (ncaa-1,ncab) then ((nca,ncb),nr+1) else if (nca,ncb) == (ncba,ncbb-1) && (nca,ncb) == (ncaa,ncab) then rank else if (nca,ncb) == (ncba,ncbb-1) then ((nca,ncb),nr+1) else if (nca,ncb) == (ncaa,ncab) || (nca,ncb) == (ncba,ncbb) then ((nca,ncb),nr-1) else rank | rank@((nca,ncb),nr) <- toList state])])
    combine (totalLoseProb,totalTransProbs) (drawLoseCounts,drawTransCounts) =
        (totalLoseProb+fromIntegral drawLoseCounts/deckSize^2,foldl addTransProb totalTransProbs drawTransCounts)
    addTransProb totalTransProbs (newStateCount,newState) = alter (maybe (if newStateCount == 0 then Nothing else Just (fromIntegral newStateCount/deckSize^2)) (Just . (+(fromIntegral newStateCount/deckSize^2)))) newState totalTransProbs

ft :: Int -> Int -> (Rational,Double)
ft nsuits nranks = (1-p,1-fromRational p)
  where
    p = table!initialState nsuits nranks
    table = fromList [(state,prob state) | state <- states nsuits nranks]
    terminals = makeTerminals (states nsuits nranks)
    prob state = lose + sum [probability*table!newState | (newState,probability) <- toList trans]
      where (lose,trans) = transitions terminals state
