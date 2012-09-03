import Data.Ratio((%));main=(putStrLn.fst.head.filter((==2012%1).snd).concatMap o.g 4)['1'..'9'];g n l|n<=1=[[l]]|otherwise=concat[map(take i l:)(g(n-1)(drop i l))|i<-[1..length l-n+1]];o n=p("",0%1)n("",(+))where p(y,c)n(a,op)|null n=[(y,c)]|otherwise=concatMap(p(y++a++head n,op c((fromIntegral.read.head)n))(tail n))[("+",(+)),("-",(-)),("*",(*)),("/",(/))]

{-
groups :: Int -> [a] -> [[[a]]]
groups n l | n <= 1 = [[l]]
           | otherwise = concat [map (take i l :) (groups (n-1) (drop i l)) | i <- [1..length l-n+1]]

opcombos :: [String] -> [(String,Rational)]
opcombos operands = opcombos' ("",0%1) operands ("",(+))
  where
    opcombos' (display,accumulator) operands (opname,op)
      | null operands = [(display,accumulator)]
      | otherwise = concatMap (opcombos' (display ++ opname ++ head operands,op accumulator ((fromIntegral . read . head) operands)) (tail operands)) [("+",(+)),("-",(-)),("*",(*)),("/",(/))]

main = (putStrLn . fst . head . filter ((== 2012%1) . snd) . concatMap opcombos . groups 4) ['1'..'9']
-}
