try :: String -> [String]
try w@[a,b,c,_,d] | c == d = [w ++ [' ',b,pred a]]
try _ = []

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . concatMap try . lines

-- queue up
