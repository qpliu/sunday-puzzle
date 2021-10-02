import Data.Map(Map,adjust,alter,empty,fromList,size)

data Ranger = A | B | C | D deriving (Bounded,Enum,Eq,Ord,Show)

type Week = ((Ranger,Ranger),(Ranger,Ranger))

type Schedule = [Week]

type Stats = (Map (Ranger,Bool) Int,Map (Ranger,Ranger) Int,Map Ranger Int,Map (Ranger,Ranger,Bool) Int)

nexts :: Week -> [Week]
nexts ((a,b),(c,d)) = [((min a c,max a c),(min b d,max b d)),((min b d,max b d),(min a c,max a c)),((min a d,max a d),(min b c,max b c)),((min b c,max b c),(min a d,max a d))]

updateStats :: Stats -> Week -> Week -> Stats
updateStats (locations,partners,moves,ploc) ((a1,b1),(c1,d1)) ((a2,b2),(c2,d2)) =
  (adjust (+1) (a2,True) $ adjust (+1) (b2,True) $ adjust (+1) (c2,False) $ adjust (+1) (d2,False) locations,
   adjust (+1) (a2,b2) $ adjust (+1) (c2,d2) partners,
   (if a1 == c2 || a1 == d2 then adjust (+1) a1 else id) $ (if b1 == c2 || b1 == d2 then adjust (+1) b1 else id) $ (if c1 == a2 || c1 == b2 then adjust (+1) c1 else id) $ (if d1 == a2 || d1 == b2 then adjust (+1) d1 else id) moves,
   adjust (+1) (a2,b2,True) $ adjust (+1) (c2,d2,False) ploc)

conditions :: Int -> Int -> Int -> Stats -> Bool
conditions maxLocations maxPartners maxMoves (locations,partners,moves,_) =
  all (<= maxLocations) locations && all (<= maxPartners) partners && all (<= maxMoves) moves

start :: (Schedule,Stats)
start = ([((A,B),(C,D))],(fromList [((r,l),0) | r <- [A .. D], l <- [True,False]],fromList [((r1,r2),0) | r1 <- [A .. C], r2 <- [succ r1 .. D]], fromList [(r,0) | r <- [A .. D]],fromList [((r1,r2,l),0) | r1 <- [A .. C], r2 <- [succ r1 .. D], l <- [True,False]]))

schedules :: Int -> [(Schedule,Stats)]
schedules maxWeeks = nextWeeks maxWeeks [start]
  where
    conds = conditions (maxWeeks `div` 2) (maxWeeks `div` 3) (maxWeeks `div` 2)
    nextWeeks :: Int -> [(Schedule,Stats)] -> [(Schedule,Stats)]
    nextWeeks remainingWeeks weeks
      | remainingWeeks <= 0 = filter ((== ((A,B),(C,D))) . head . fst) weeks
      | otherwise = nextWeeks (remainingWeeks-1) (concatMap nextWeek weeks)
    nextWeek :: (Schedule,Stats) -> [(Schedule,Stats)]
    nextWeek (sched@(week:_),stats) =
      [(next:sched,updateStats stats week next) | next <- nexts week, conds (updateStats stats week next)]

main :: IO ()
main = do
  let skeds = schedules 12
  print (length skeds)
  mapM_ print (fst (head skeds))
  let stats = foldr (alter (Just . maybe 1 (+1))) empty $ map snd skeds
  print stats
