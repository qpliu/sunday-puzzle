import Data.Set(Set,delete,difference,fromList,insert,member)

p2 = concat [
    "..." ++ "#.." ++ "..#",
    ".#." ++ "..#" ++ "...",
    "##." ++ "..." ++ "#..",

    "..#" ++ ".#." ++ "...",
    "..#" ++ "..#" ++ ".##",
    "#.#" ++ "###" ++ "...",

    "#.." ++ "##." ++ "..#",
    ".##" ++ "..#" ++ "#..",
    "#.#" ++ "..." ++ "..."
    ]

p2solution = concat [
    "462" ++ "197" ++ "853",
    "957" ++ "832" ++ "614",
    "381" ++ "564" ++ "792",

    "894" ++ "251" ++ "376",
    "275" ++ "346" ++ "189",
    "613" ++ "789" ++ "245",

    "138" ++ "425" ++ "967",
    "526" ++ "978" ++ "431",
    "749" ++ "613" ++ "528"
    ]

p3 = concat [
    "..." ++ ".#." ++ "#..",
    "..#" ++ "..." ++ "###",
    "..#" ++ "..." ++ "...",

    ".##" ++ ".##" ++ "...",
    "..." ++ "..." ++ ".##",
    "#.." ++ "##." ++ ".##",

    "..." ++ ".##" ++ ".#.",
    "..." ++ "..." ++ "#..",
    "##." ++ "#.." ++ ".#."
    ]

p3solution = concat [
    "478" ++ "529" ++ "136",
    "916" ++ "348" ++ "752",
    "253" ++ "617" ++ "948",

    "824" ++ "956" ++ "317",
    "697" ++ "134" ++ "285",
    "135" ++ "782" ++ "469",

    "549" ++ "873" ++ "621",
    "361" ++ "295" ++ "874",
    "782" ++ "461" ++ "593"
    ]

keys = [fromList [k `mod` 9 + 1,k `div` 9 + 1,h k] | k <- [0..80]]
  where
    h k = 1 + dx + 3*dy
      where
        x = k `mod` 9
        y = k `div` 9
        x0 = x `div` 3
        y0 = y `div` 3
        dx = x - 3*x0
        dy = y - 3*y0

puz :: String -> [Set Int]
puz p = zipWith cell p keys
  where
    cell '.' k = difference (fromList [1..9]) k
    cell '#' k = k

rm :: [Set Int] -> Int -> Int -> [Set Int]
rm p i n = map f $ zip [1..] p
  where
    f (j,cell) | i == j = delete n cell | otherwise = cell

add :: [Set Int] -> Int -> Int -> [Set Int]
add p i n = map f $ zip [1..] p
  where
    f (j,cell) | i == j = insert n cell | otherwise = cell

play :: [Set Int] -> IO ()
play p = do
    disp p
    l <- getLine
    let w = words l
    if ["q"] == w
      then print p
      else
        if ["+"] == take 1 w
          then
            let [i,n] = map read $ drop 1 w
            in  play (add p i n)
          else
            let [i,n] = map read w
            in  play (rm p i n)

disp :: [Set Int] -> IO ()
disp p = dispRows 1 p
  where
    dispRows _ [] = return ()
    dispRows i cells = do
        if i == 28 || i == 55
          then putStrLn (take 63 $ repeat '-')
          else return ()
        dispRow i (take 9 cells)
        dispRows (i+9) (drop 9 cells)
    dispRow i cells = do
        dispRow1 i cells
        putStrLn ""
        dispRow2 i cells
        putStrLn ""
        dispRow3 i cells
        putStrLn ""
    dispRow1 _ [] = return ()
    dispRow1 i (cell:cells) = do
        if ((i-1) `mod` 9) `elem` [3,6] then putStr "|" else putStr " "
        if i < 10 then putStr " " else putStr ""
        putStr (show i)
        putStr ":"
        if 1 `member` cell then putStr "1" else putStr " "
        if 2 `member` cell then putStr "2" else putStr " "
        if 3 `member` cell then putStr "3" else putStr " "
        dispRow1 (i+1) cells
    dispRow2 _ [] = return ()
    dispRow2 i (cell:cells) = do
        if ((i-1) `mod` 9) `elem` [3,6] then putStr "|" else putStr " "
        putStr "   "
        if 4 `member` cell then putStr "4" else putStr " "
        if 5 `member` cell then putStr "5" else putStr " "
        if 6 `member` cell then putStr "6" else putStr " "
        dispRow2 (i+1) cells
    dispRow3 _ [] = return ()
    dispRow3 i (cell:cells) = do
        if ((i-1) `mod` 9) `elem` [3,6] then putStr "|" else putStr " "
        putStr "   "
        if 7 `member` cell then putStr "7" else putStr " "
        if 8 `member` cell then putStr "8" else putStr " "
        if 9 `member` cell then putStr "9" else putStr " "
        dispRow3 (i+1) cells
