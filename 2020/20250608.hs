import Data.Set(Set,empty,insert,member)

matches :: Set String -> String -> [(String,String)]
matches set s = map ((,) s) $ filter (`member` set) $ m "" s
  where
    m pre "" = []
    m "" (l:ls) = m [l] ls
    m pre ('t':ls@(_:_)) =
        (reverse pre ++ ls) : (reverse pre ++ "tt" ++ ls) : m ('t':pre) ls
    m pre (l:ls) = (reverse pre ++ "t" ++ l:ls) : m (l:pre) ls

scan :: Set String -> [String] -> [(String,String)]
scan _ [] = []
scan set (w:ws) =  matches set w ++ scan (insert w set) ws

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . scan empty . words

-- does dotes fawns
