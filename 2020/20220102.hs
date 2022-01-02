import Data.Set(Set,empty,insert,member)

op :: String -> String
op (l1:l2:l3:l4:l5:l6:l7:ls) = l7:l5:l6:l1:l2:l3:l4:ls
op _ = ""

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . filter (not . null) . map op . lines

-- kohlrabi broccoli
