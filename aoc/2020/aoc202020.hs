-- For part 1, just finding the corner tiles is sufficient.

-- I'll assume the IDs are unique.  I doubt that they won't be, but if they
-- turn out not be unique, I can generate a unique one by incrementing a
-- counter when parsing.

-- I'll assume all borders are different.  If not, constructing the image
-- will become a depth-first search.

-- I'll assume the borders of a tile are all different.  If not, I'll have
-- to rethink.

import Data.Bits(clearBit,setBit,testBit)
import Data.Map(Map,alter,empty,insert,toList,(!))
import qualified Data.Map
import Data.Set(Set,elems,fromList,intersection,member,size)
import qualified Data.Set

newtype Tile = Tile (Int,[Int]) deriving (Eq,Ord,Show)
newtype Border = Border Int deriving (Eq,Ord,Show)
type Borders = (Border,Border,Border,Border)
type Connections = Map Border (Set Tile)

parse :: String -> [Tile]
parse [] = []
parse ('T':'i':'l':'e':' ':rest) = parseTileID rest
  where
    parseTileID str = parseTile (read tileid) (drop 2 rest)
      where (tileid,rest) = span (/= ':') str
    parseTile tileid = p 0 0 []
      where
        p x row rows [] = [Tile (tileid,(row:rows))]
        p x row rows ('\n':[]) = [Tile (tileid,(row:rows))]
        p x row rows ('\n':'\n':rest) = Tile (tileid,(row:rows)) : parse rest
        p x row rows ('\n':rest) = p 0 0 (row:rows) rest
        p x row rows ('#':rest) = p (x+1) (setBit row x) rows rest
        p x row rows (_:rest) = p (x+1) row rows rest

tileID :: Tile -> Int
tileID (Tile (tileid,_)) = tileid

borders :: Tile -> Borders
borders (Tile (_,rows)) =
    (makeBorder (head rows),
     makeBorder (columnBits rows 0),
     makeBorder (columnBits rows (len - 1)),
     makeBorder (last rows))
  where
    len = length rows
    makeBorder x = Border (min x y) where y = reverseBits len x
    reverseBits len bits = foldr copyBit 0 [0..len-1]
      where
        copyBit bit result
          | testBit bits bit = setBit result (len-1 - bit)
          | otherwise = result

columnBits :: [Int] -> Int -> Int
columnBits rows col = foldr copyBit 0 $ zip [0..] rows
  where
    copyBit (bit,row) result
      | testBit row col = setBit result bit
      | otherwise = result

borderOrientations :: Borders -> [Borders]
borderOrientations borders = rotations ++ map flipped rotations
  where
    rotations = take 4 $ iterate rotate borders
    rotate (top,left,right,bottom) = (left,bottom,top,right)
    flipped (top,left,right,bottom) = (top,right,left,bottom)

makeConnections :: [Tile] -> Connections
makeConnections tiles = foldr addTile empty tiles
  where
    addTile tile connections = foldr addConnection connections [t,l,r,b]
      where
        (t,l,r,b) = borders tile
        addConnection = alter (Just . maybe (fromList [tile]) (Data.Set.insert tile))

isOuter :: Connections -> Border -> Bool
isOuter connections border = size (connections!border) == 1

isCorner :: Connections -> Borders -> Bool
isCorner connections (top,left,right,bottom) = (isOuter connections top || isOuter connections bottom) && (isOuter connections left || isOuter connections right)

getCorners :: Connections -> [Tile] -> [Tile]
getCorners connections = filter (isCorner connections . borders)

getCornerIDs :: String -> [Int]
getCornerIDs input = map tileID $ getCorners connections tiles
  where
    tiles = parse input
    connections = makeConnections tiles

getConnected :: Connections -> Tile -> Border -> Tile
getConnected connections tile border = head $ filter (/= tile) $ elems $ (connections!border)

testData :: String
testData = "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###\n\nTile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..\n\nTile 1171:\n####...##.\n#..##.#..#\n##.#..#.#.\n.###.####.\n..###.####\n.##....##.\n.#...####.\n#.##.####.\n####..#...\n.....##...\n\nTile 1427:\n###.##.#..\n.#..#.##..\n.#.##.#..#\n#.#.#.##.#\n....#...##\n...##..##.\n...#.#####\n.#.####.#.\n..#..###.#\n..##.#..#.\n\nTile 1489:\n##.#.#....\n..##...#..\n.##..##...\n..#...#...\n#####...#.\n#..#.#.#.#\n...#.#.#..\n##.#...##.\n..##.##.##\n###.##.#..\n\nTile 2473:\n#....####.\n#..#.##...\n#.##..#...\n######.#.#\n.#...#.#.#\n.#########\n.###.#..#.\n########.#\n##...##.#.\n..###.#.#.\n\nTile 2971:\n..#.#....#\n#...###...\n#.#.###...\n##.##..#..\n.#####..##\n.#..####.#\n#..#.#..#.\n..####.###\n..#.#.###.\n...#.#.#.#\n\nTile 2729:\n...#.#.#.#\n####.#....\n..#.#.....\n....#..#.#\n.##..##.#.\n.#.####...\n####.#.#..\n##.####...\n##..#.##..\n#.##...##.\n\nTile 3079:\n#.#.#####.\n.#..######\n..#.......\n######....\n####.#..#.\n.#...#.##.\n#.#####.##\n..#.###...\n..#.......\n..#.###...\n"

test :: ()
test
  | (product . getCornerIDs) testData /= 20899048083289 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (product . getCornerIDs) $ readFile "input/20.txt"

type UnorientedTile = (Tile,Border,Border) -- tile,right border,bottom border
type UnorientedImage = Map (Int,Int) UnorientedTile

makeImage :: [Tile] -> UnorientedImage
makeImage tiles = buildTopRow 0 start start empty
  where
    start = orient outer outer $ head $ getCorners connections tiles
    connections = makeConnections tiles
    outer = isOuter connections
    orient testTop testLeft tile = (tile,r,b)
      where
        (_,_,r,b) = head $ filter test $ borderOrientations $ borders tile
        test (t,l,_,_) = testTop t && testLeft l
    buildTopRow x otile0@(tile0,_,bottom0) otile@(tile,right,_) image
      | outer right = buildRow 0 1 next0 next0 (insert (x,0) otile image)
      | otherwise = buildTopRow (x+1) otile0 next (insert (x,0) otile image)
      where
        next = orient outer (== right) $ getConnected connections tile right
        next0 = orient (== bottom0) outer $ getConnected connections tile0 bottom0
    buildRow x y otile0@(tile0,_,bottom0) otile@(tile,right,_) image
      | outer right && outer bottom0 = insert (x,y) otile image
      | outer right = buildRow 0 (y+1) next0 next0 (insert (x,y) otile image)
      | otherwise = buildRow (x+1) y otile0 next (insert (x,y) otile image)
      where
        (_,_,bottomx) = image!(x+1,y-1)
        next = orient (== bottomx) (== right) $ getConnected connections tile right
        next0 = orient (== bottom0) outer $ getConnected connections tile0 bottom0

type Image = Map (Int,Int) Tile

orientTiles :: UnorientedImage -> Image
orientTiles = Data.Map.map orientTile

orientTile :: UnorientedTile -> Tile
orientTile (tile@(Tile (tileid,rows)),right,bottom) 
  | r /= right = orientTile (rotateTile tile,right,bottom)
  | b /= bottom = Tile (tileid,reverse rows)
  | otherwise = tile
  where
    (_,_,r,b) = borders (Tile (tileid,rows))

rotateTile :: Tile -> Tile
rotateTile (Tile (tileid,rows)) =
    Tile (tileid,[columnBits rows col | col <- [len - 1,len - 2..0]])
  where len = length rows

monsterString :: [String]
monsterString = [
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
    ]

monsters :: [Set (Int,Int)] -- in all orientations
monsters = (take 4 . iterate rotMon . makeMon) monsterString
        ++ (take 4 . iterate rotMon . makeMon . reverse) monsterString
  where
    makeMon strs = fromList $ concat $ zipWith makeMonRow [0..] strs
    makeMonRow y str = map (fmap $ const y) $ filter ((== '#') . snd) $ zip [0..] str
    rotMon = Data.Set.map rotxy
    rotxy (x,y) = (y,-x)

display :: Set (Int,Int) -> String
display im = unlines $ [[if member (x,y) im then '#' else '.' | x <- [xmin..xmax]] | y <- [ymin..ymax]]
  where
    xmin = minimum $ Data.Set.map fst im
    xmax = maximum $ Data.Set.map fst im
    ymin = minimum $ Data.Set.map snd im
    ymax = maximum $ Data.Set.map snd im

-- Since the tiles are all 10x10, make that assumption here.
-- And there are 144 tiles, making for a 96x96 image after
-- removing the borders.
toImg :: Image -> Set (Int,Int)
toImg = fromList . concatMap toPoints . toList
  where
    toPoints ((tilex,tiley),Tile (_,rows)) = [(tilex*8+x,tiley*8+y) | x <- [0..7], (y,row) <- zip [0..] (init $ tail rows), testBit row (x+1)]

translate :: Set (Int,Int) -> (Int,Int) -> Set (Int,Int)
translate img (dx,dy) = Data.Set.map xlate img
  where xlate (x,y) = (x+dx,y+dy)

findMonsters :: Set (Int,Int) -> Set (Int,Int) -> [(Int,Int)]
findMonsters img monster = [(x,y) | x <- [0..95], y <- [0..95], hasMon (x,y)]
  where
    hasMon xy = intersection img xmon == xmon where xmon = translate monster xy

countMonsters :: Set (Int,Int) -> Int
countMonsters img = maximum $ map (length . findMonsters img) monsters

-- Size of monster is 15.  I'll assume there are no overlapping monsters.
part2 :: IO Int
part2 = do
    img <- fmap (toImg . orientTiles . makeImage . parse) $ readFile "input/20.txt"
    return $ size img - 15*countMonsters img
