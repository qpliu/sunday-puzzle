import Data.Map(Map,fromList,toList,(!))

first :: [(Int,Rational,Double)]
first = map (\ ((_,_,slugger),runs) -> (slugger,runs,fromRational runs)) $ filter (\ ((runners,outs,_),_) -> runners == 0 && outs == 0) $ toList memo
  where
    memo = fromList [((runners,outs,slugger),r runners outs slugger) | runners <- [0..3], outs <- [0..3], slugger <- [1..9]]
    r runners outs slugger
      | outs >= 3 = 0
      | slugger == 1 && runners >= 3 = 1/(1 - 1/(10*3^8))*(memo!(3,outs+1,9)*9/10 + 1/10*(4 + 2/3*memo!(0,outs+1,9)
            + 1/3*(2/3*memo!(1,outs+1,8)
            + 1/3*(2/3*memo!(2,outs+1,7)
            + 1/3*(2/3*memo!(3,outs+1,6)
            + 1/3*(1 + 2/3*memo!(3,outs+1,5)
            + 1/3*(1 + 2/3*memo!(3,outs+1,4)
            + 1/3*(1 + 2/3*memo!(3,outs+1,3)
            + 1/3*(1 + 2/3*memo!(3,outs+1,1))))))))))
      | slugger == 1 = (1 + fromIntegral runners + memo!(0,outs,9))/10 + memo!(runners,outs+1,9)*9/10
      | runners >= 3 = (1 + memo!(3,outs,slugger-1))/3 + memo!(3,outs+1,slugger-1)*2/3
      | otherwise = memo!(runners+1,outs,slugger-1)/3 + memo!(runners,outs+1,slugger-1)*2/3

-- for slugger slots at the start of the inning, return the cases
-- the slugger slots at end of the inning,
-- average number of runs scored in the inning in this case, and the
-- probability of this case
inningOutcomes :: (Int,Int) -> Map (Int,Int) (Rational,Rational)
inningOutcomes sluggersStart@(slugger1start,slugger2start) =
    fromList $ map (\ ((_,_,_,sluggersEnd),runsProb) -> (sluggersEnd,runsProb)) $ filter (\ ((runners,outs,sluggers,_),_) -> runners == 0 && outs == 0 && sluggers == sluggersStart) $ toList memo
  where
    memo = fromList [((runners,outs,sluggers,sluggersEnd),runsProbs runners outs sluggers sluggersEnd) | runners <- [0..3], outs <- [0..3], sluggers <- sluggersCases, sluggersEnd <- sluggersCases]
    sluggersCases = [
        let s1 = (slugger1start+i) `mod` 9 + 1
            s2 = (slugger2start+i) `mod` 9 + 1
        in  (min s1 s2,max s1 s2)
        | i <- [1..9]]
    runsProbs runners outs sluggers@(slugger1,slugger2) sluggersEnd
      | slugger1 >= slugger2 = error (show "sluggers="++show (slugger1,slugger2))
      | outs >= 3 && sluggers == sluggersEnd = (0,1)
      | outs >= 3 {- sluggers /= sluggersEnd -} = (0,0)
      | slugger1 > 1 && runners < 3 =
          join 1
              (2/3,memo!(runners,outs+1,(slugger1-1,slugger2-1),sluggersEnd))
              (1/3,0,memo!(runners+1,outs,(slugger1-1,slugger2-1),sluggersEnd))
      | slugger1 > 1 {- runners == 3 -} =
          join 1
              (2/3,memo!(3,outs+1,(slugger1-1,slugger2-1),sluggersEnd))
              (1/3,1,memo!(3,outs,(slugger1-1,slugger2-1),sluggersEnd))
      | runners < 3 {- slugger1 == 1 -} =
          join 1
              (9/10,memo!(runners,outs+1,(slugger2-1,9),sluggersEnd))
              (1/10,1+fromIntegral runners,memo!(0,outs,(slugger2-1,9),sluggersEnd))
      | slugger2 == 2 {- runners == 3 slugger1 == 1 -} =
          join (1/(1 - 1/(10^2*3^7)))
            (9/10,memo!(3,outs+1,(1,9),sluggersEnd))
            (1/10,4,join 1 -- 2 (slugger2)
              (9/10,memo!(0,outs+1,(8,9),sluggersEnd))
              (1/10,1,(join 1 -- 3
                (2/3,memo!(0,outs+1,(7,8),sluggersEnd))
                (1/3,0,(join 1 -- 4
                  (2/3,memo!(1,outs+1,(6,7),sluggersEnd))
                  (1/3,0,(join 1 -- 5
                    (2/3,memo!(2,outs+1,(5,6),sluggersEnd))
                    (1/3,0,(join 1 -- 6
                      (2/3,memo!(3,outs+1,(4,5),sluggersEnd))
                      (1/3,1,(join 1 -- 7
                         (2/3,memo!(3,outs+1,(3,4),sluggersEnd))
                         (1/3,1,(join 1 -- 8
                            (2/3,memo!(3,outs+1,(2,3),sluggersEnd))
                            (1/3,1,scale (2/3) (memo!(3,outs+1,(1,2),sluggersEnd)))))))))))))))) -- 9
      | slugger2 == 3 {- runners == 3 slugger1 == 1 -} =
          join (1/(1 - 1/(10^2*3^7)))
            (9/10,memo!(3,outs+1,(2,9),sluggersEnd))
            (1/10,4,join 1 -- 2
              (2/3,memo!(0,outs+1,(1,8),sluggersEnd))
              (1/3,0,(join 1 -- 3 (slugger2)
                (9/10,memo!(1,outs+1,(7,9),sluggersEnd))
                (1/10,2,(join 1 -- 4
                  (2/3,memo!(0,outs+1,(6,8),sluggersEnd))
                  (1/3,0,(join 1 -- 5
                    (2/3,memo!(1,outs+1,(5,7),sluggersEnd))
                    (1/3,0,(join 1 -- 6
                      (2/3,memo!(2,outs+1,(4,6),sluggersEnd))
                      (1/3,0,(join 1 -- 7
                         (2/3,memo!(3,outs+1,(3,5),sluggersEnd))
                         (1/3,1,(join 1 -- 8
                            (2/3,memo!(3,outs+1,(2,4),sluggersEnd))
                            (1/3,1,scale (2/3) (memo!(3,outs+1,(2,4),sluggersEnd)))))))))))))))) -- 9
      | slugger2 == 4 {- runners == 3 slugger1 == 1 -} =
          join (1/(1 - 1/(10^2*3^7)))
            (9/10,memo!(3,outs+1,(3,9),sluggersEnd))
            (1/10,4,join 1 -- 2
              (2/3,memo!(0,outs+1,(2,8),sluggersEnd))
              (1/3,0,(join 1 -- 3
                (2/3,memo!(1,outs+1,(1,7),sluggersEnd))
                (1/3,0,(join 1 -- 4 (slugger2)
                  (9/10,memo!(2,outs+1,(6,9),sluggersEnd))
                  (1/10,3,(join 1 -- 5
                    (2/3,memo!(0,outs+1,(5,8),sluggersEnd))
                    (1/3,0,(join 1 -- 6
                      (2/3,memo!(1,outs+1,(4,7),sluggersEnd))
                      (1/3,0,(join 1 -- 7
                         (2/3,memo!(2,outs+1,(3,6),sluggersEnd))
                         (1/3,0,(join 1 -- 8
                            (2/3,memo!(3,outs+1,(2,5),sluggersEnd))
                            (1/3,1,scale (2/3) (memo!(3,outs+1,(2,5),sluggersEnd)))))))))))))))) -- 9
      | slugger2 == 5 {- runners == 3 slugger1 == 1 -} =
          join (1/(1 - 1/(10^2*3^7)))
            (9/10,memo!(3,outs+1,(4,9),sluggersEnd))
            (1/10,4,join 1 -- 2
              (2/3,memo!(0,outs+1,(3,8),sluggersEnd))
              (1/3,0,(join 1 -- 3
                (2/3,memo!(1,outs+1,(2,7),sluggersEnd))
                (1/3,0,(join 1 -- 4
                  (2/3,memo!(2,outs+1,(1,6),sluggersEnd))
                  (1/3,0,(join 1 -- 5 (slugger2)
                    (9/10,memo!(3,outs+1,(5,9),sluggersEnd))
                    (1/10,0,(join 1 -- 6
                      (2/3,memo!(0,outs+1,(4,8),sluggersEnd))
                      (1/3,0,(join 1 -- 7
                         (2/3,memo!(1,outs+1,(3,7),sluggersEnd))
                         (1/3,0,(join 1 -- 8
                            (2/3,memo!(2,outs+1,(2,6),sluggersEnd))
                            (1/3,0,scale (2/3) (memo!(3,outs+1,(2,6),sluggersEnd)))))))))))))))) -- 9
      | otherwise {- runners == 3 slugger1 == 1 slugger2 > 5 -} =
          join 1
              (9/10,memo!(3,outs+1,(slugger2-1,9),sluggersEnd))
              (1/10,4,memo!(0,outs,(slugger2-1,9),sluggersEnd))
      where
        join factor (probout,(runsafterout,probafterout)) (probhit,runshit,(runsafterhit,probafterhit)) =
            (factor*(probhit*(runshit+runsafterhit)+probout*runsafterout),
             factor*(probhit*probafterhit+probout*probafterout))
        scale factor (runs,prob) = (factor*runs,factor*prob)

inningOutcomesMemo :: Map (Int,Int) (Map (Int,Int) (Rational,Rational))
inningOutcomesMemo = fromList [(sluggers,inningOutcomes sluggers) | sluggers <- [(s1,s2) | s1 <- [1..8], s2 <- [s1+1..9]]]

runs :: Int -> (Int,Int) -> Rational
runs inning sluggers = memo!(inning,sluggers)
  where
    memo = fromList [((i,s),r i s) | i <- [1..10], s <- [(s1,s2) | s1 <- [1..8], s2 <- [s1+1..9]]]
    r i s
      | i > 9 = 0
      | otherwise = sum [prob*(runs1 + memo!(i+1,sluggers1)) | (sluggers1,(runs1,prob)) <- toList (inningOutcomesMemo!s)]

main :: IO ()
main = do
    mapM_ print first
    putStrLn ""
    mapM_ print [(slugger1,slugger2,fromRational r :: Double,r) | slugger1 <- [1..8], slugger2 <- [slugger1+1 .. 9], r <- [runs 1 (slugger1,slugger2)]]
