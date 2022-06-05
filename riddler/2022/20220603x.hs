import Data.Set(Set,fromList,member,toList)

splits :: [a] -> [([a],[a])]
splits [] = []
splits [a,b] = [([a],[b])]
splits (a:as) = ([a],as) : concat [[(a:xs,ys),(xs,a:ys)] | (xs,ys) <- splits as]

makes :: [Int] -> Set Int
makes [] = fromList []
makes [a] = fromList [a]
makes as = fromList $ concat [[x,y,x+y,x*y] | (xs,ys) <- splits as, x <- toList (makes xs), y <- toList (makes ys)]

fun :: Int -> Int
fun n = head $ filter (not . flip member (makes [1..n])) [1..]

main :: IO ()
main = mapM_ print [(n,fun n) | n <- [1..8]]
