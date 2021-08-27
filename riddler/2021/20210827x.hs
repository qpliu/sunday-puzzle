import Data.Foldable(maximumBy)

data QB = QB1 | QB2 | QB3 deriving Show
data RB = RB1 | RB2 | RB3 deriving Show
data WR = WR1 | WR2 | WR3 deriving Show

data Board = Board [QB] [RB] [WR] deriving Show

data Picks = Picks (Maybe QB) (Maybe RB) (Maybe WR) deriving Show

points :: Picks -> Int
points (Picks qb rb wr) = maybe 0 qbp qb + maybe 0 rbp rb + maybe 0 wrp wr
  where
    qbp QB1 = 400
    qbp QB2 = 350
    qbp QB3 = 300
    rbp RB1 = 300
    rbp RB2 = 225
    rbp RB3 = 200
    wrp WR1 = 250
    wrp WR2 = 225
    wrp WR3 = 175

pickA :: ((Picks,Picks,Picks,Board) -> (Picks,Picks,Picks,Board)) -> (Picks,Picks,Picks,Board) -> (Picks,Picks,Picks,Board)
pickA next (Picks qb rb wr,b,c,Board qbs rbs wrs) = maximumBy compA $
    maybe [next (Picks (Just (head qbs)) rb wr,b,c,Board (tail qbs) rbs wrs)]
          (const []) qb ++
    maybe [next (Picks qb (Just (head rbs)) wr,b,c,Board qbs (tail rbs) wrs)]
          (const []) rb ++
    maybe [next (Picks qb rb (Just (head wrs)),b,c,Board qbs rbs (tail wrs))]
          (const []) wr
  where
    compA (a1,_,_,_) (a2,_,_,_) = compare (points a1) (points a2)

pickB :: ((Picks,Picks,Picks,Board) -> (Picks,Picks,Picks,Board)) -> (Picks,Picks,Picks,Board) -> (Picks,Picks,Picks,Board)
pickB next (a,Picks qb rb wr,c,Board qbs rbs wrs) = maximumBy compB $
    maybe [next (a,Picks (Just (head qbs)) rb wr,c,Board (tail qbs) rbs wrs)]
          (const []) qb ++
    maybe [next (a,Picks qb (Just (head rbs)) wr,c,Board qbs (tail rbs) wrs)]
          (const []) rb ++
    maybe [next (a,Picks qb rb (Just (head wrs)),c,Board qbs rbs (tail wrs))]
          (const []) wr
  where
    compB (_,b1,_,_) (_,b2,_,_) = compare (points b1) (points b2)

pickC :: ((Picks,Picks,Picks,Board) -> (Picks,Picks,Picks,Board)) -> (Picks,Picks,Picks,Board) -> (Picks,Picks,Picks,Board)
pickC next (a,b,Picks qb rb wr,Board qbs rbs wrs) = maximumBy compC $
    maybe [next (a,b,Picks (Just (head qbs)) rb wr,Board (tail qbs) rbs wrs)]
          (const []) qb ++
    maybe [next (a,b,Picks qb (Just (head rbs)) wr,Board qbs (tail rbs) wrs)]
          (const []) rb ++
    maybe [next (a,b,Picks qb rb (Just (head wrs)),Board qbs rbs (tail wrs))]
          (const []) wr
  where
    compC (_,_,c1,_) (_,_,c2,_) = compare (points c1) (points c2)

main :: IO ()
main = do
    let p0 = Picks Nothing Nothing Nothing
    let draft2 = pickB (pickC (pickC (pickB (pickA (pickA (pickB (pickC id)))))))
    let draft1 = pickA (draft2)
    let pr draft board = let (a,b,c,_) = draft board in print (points a,a,points b,b,points c,c)
    pr draft1 (p0,p0,p0,Board [QB1,QB2,QB3] [RB1,RB2,RB3] [WR1,WR2,WR3])
    -- force first pick to be QB1
    pr draft2 (Picks (Just QB1) Nothing Nothing,p0,p0,Board [QB2,QB3] [RB1,RB2,RB3] [WR1,WR2,WR3])
    -- force first pick to be RB1
    pr draft2 (Picks Nothing (Just RB1) Nothing,p0,p0,Board [QB1,QB2,QB3] [RB2,RB3] [WR1,WR2,WR3])
