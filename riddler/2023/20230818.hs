import Data.Map(Map,fromList,toList,(!))

first :: [(Int,Rational,Double)]
first = map (\ ((_,_,slugger),runs) -> (slugger,runs,fromRational runs)) $ filter (\ ((runners,outs,_),_) -> runners == 0 && outs == 0) $ toList memo
  where
    memo = fromList [((runners,outs,slugger),r runners outs slugger) | runners <- [0..3], outs <- [0..3], slugger <- [1..9]]
    r runners outs slugger
      | outs == 3 = 0
      | slugger == 1 && runners == 3 = (1 - 1/(10*3^8))*(memo!(3,outs+1,9)*9/10 + 1/10*(4 + 2/3*memo!(0,outs+1,9)
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

main :: IO ()
main = do
    mapM_ print first
