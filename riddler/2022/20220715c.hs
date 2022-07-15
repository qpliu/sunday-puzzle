import Data.Set(Set,fromList,member,size)

type Cube = [String]

rot :: Char -> Char
rot '^' = '>'
rot '>' = 'v'
rot 'v' = '<'
rot '<' = '^'
rot '|' = '-'
rot '-' = '|'
rot '/' = '\\'
rot '\\' = '/'
rot c = c

--    cv               b>
-- b^ a< dv f<  ->  e< a^ c< fv
--    e>               d<
rotx :: Cube -> Cube
rotx [a,b,c,d,e,f] =
    [map rot a,map rot e,map rot b,map rot c,map rot d,map (rot . rot . rot) f]

--    cv               a<
-- b^ a< dv f<  ->  b< ev d< c^
--    e>               f>
roty :: Cube -> Cube
roty [a,b,c,d,e,f] =
    [e,map (rot . rot . rot) b,a,map rot d,map (rot . rot) f,map (rot . rot) c]

rots :: Cube -> [Cube]
rots cube = rotxs cube -- a on top
        ++ (rotxs . roty . rotx . rotx . rotx) cube -- b on top
        ++ (rotxs . roty . rotx . rotx) cube -- c on top
        ++ (rotxs . roty . rotx) cube -- d on top
        ++ (rotxs . roty) cube -- e on top
        ++ (rotxs . roty . roty) cube -- f on top
  where
    rotxs = take 4 . iterate rotx

uniques :: [Cube] -> [Set Cube]
uniques [] = []
uniques (c:cs) = rotset:uniques (filter (not . (`member` rotset)) cs)
  where rotset = fromList (rots c)

countUniques :: [String] -> Int
countUniques m =
    length $ uniques $ [[a,b,c,d,e,f]|a<-m,b<-m,c<-m,d<-m,e<-m,f<-m]

main :: IO ()
main = do
    print $ countUniques ["-","|"]
    print $ countUniques ["-/","-\\","|/","|\\"]
    print $ countUniques ["-","|","/","\\"]
