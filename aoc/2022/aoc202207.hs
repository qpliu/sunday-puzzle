data Node = Dir String [Node] | File String Int deriving Show

parse :: String -> [(([String]),Either String (String,Int))]
parse = collect [] . map words . lines
  where
    collect cwd [] = []
    collect cwd (["$","cd","/"]:rest) = collect [] rest
    collect cwd (["$","cd",".."]:rest) = collect (drop 1 cwd) rest
    collect cwd (["$","cd",dir]:rest) = collect (dir:cwd) rest
    collect cwd (("$":_):rest) = collect cwd rest
    collect cwd (["dir",name]:rest) = (cwd,Left name) : collect cwd rest
    collect cwd ([size,name]:rest) = (cwd,Right (name,read size)) : collect cwd rest

node :: [String] -> [(([String]),Either String (String,Int))] -> Node
node dir ls = Dir (concat (take 1 dir)) (map (subnode . snd) $ filter ((== dir) . fst) ls)
  where
    subnode (Left subdir) = node (subdir:dir) ls
    subnode (Right (name,size)) = File name size

du :: Node -> Int
du (Dir _ entries) = sum $ map du entries
du (File _ size) = size

lsRd :: Node -> [Node]
lsRd dir@(Dir _ entries) = dir : concatMap lsRd entries
lsRd (File _ _) = []

testData :: String
testData = unlines [
    "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"
    ]

test :: ()
test
  | (sum . filter (<= 100000) . map du . lsRd . node [] . parse) testData /= 95437 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . filter (<= 100000) . map du . lsRd . node [] . parse) $ readFile "input/07.txt"

free :: Int -> Int -> Node -> Int
free capacity needed root = minimum $ filter (>= needToFree) $ map du $ lsRd root
  where needToFree = needed - (capacity - du root)

test2 :: ()
test2
  | (free 70000000 30000000 . node [] . parse) testData /= 24933642 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (free 70000000 30000000 . node [] . parse) $ readFile "input/07.txt"
