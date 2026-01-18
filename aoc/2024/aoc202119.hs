module AOC202119 where

import Data.List(sort)
import Data.Map(Map,alter,empty,findWithDefault,mergeWithKey,toList)
import Data.Set(Set,elems,fromList,intersection,size,unions)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2021/input/19",
    aocTests=[
        AOCTest {
            testData=unlines [
                "--- scanner 0 ---",
                "404,-588,-901",
                "528,-643,409",
                "-838,591,734",
                "390,-675,-793",
                "-537,-823,-458",
                "-485,-357,347",
                "-345,-311,381",
                "-661,-816,-575",
                "-876,649,763",
                "-618,-824,-621",
                "553,345,-567",
                "474,580,667",
                "-447,-329,318",
                "-584,868,-557",
                "544,-627,-890",
                "564,392,-477",
                "455,729,728",
                "-892,524,684",
                "-689,845,-530",
                "423,-701,434",
                "7,-33,-71",
                "630,319,-379",
                "443,580,662",
                "-789,900,-551",
                "459,-707,401",
                "",
                "--- scanner 1 ---",
                "686,422,578",
                "605,423,415",
                "515,917,-361",
                "-336,658,858",
                "95,138,22",
                "-476,619,847",
                "-340,-569,-846",
                "567,-361,727",
                "-460,603,-452",
                "669,-402,600",
                "729,430,532",
                "-500,-761,534",
                "-322,571,750",
                "-466,-666,-811",
                "-429,-592,574",
                "-355,545,-477",
                "703,-491,-529",
                "-328,-685,520",
                "413,935,-424",
                "-391,539,-444",
                "586,-435,557",
                "-364,-763,-893",
                "807,-499,-711",
                "755,-354,-619",
                "553,889,-390",
                "",
                "--- scanner 2 ---",
                "649,640,665",
                "682,-795,504",
                "-784,533,-524",
                "-644,584,-595",
                "-588,-843,648",
                "-30,6,44",
                "-674,560,763",
                "500,723,-460",
                "609,671,-379",
                "-555,-800,653",
                "-675,-892,-343",
                "697,-426,-610",
                "578,704,681",
                "493,664,-388",
                "-671,-858,530",
                "-667,343,800",
                "571,-461,-707",
                "-138,-166,112",
                "-889,563,-600",
                "646,-828,498",
                "640,759,510",
                "-630,509,768",
                "-681,-892,-333",
                "673,-379,-804",
                "-742,-814,-386",
                "577,-820,562",
                "",
                "--- scanner 3 ---",
                "-589,542,597",
                "605,-692,669",
                "-500,565,-823",
                "-660,373,557",
                "-458,-679,-417",
                "-488,449,543",
                "-626,468,-788",
                "338,-750,-386",
                "528,-832,-391",
                "562,-778,733",
                "-938,-730,414",
                "543,643,-506",
                "-524,371,-870",
                "407,773,750",
                "-104,29,83",
                "378,-903,-323",
                "-778,-728,485",
                "426,699,580",
                "-438,-605,-362",
                "-469,-447,-387",
                "509,732,623",
                "647,635,-688",
                "-868,-804,481",
                "614,-800,639",
                "595,780,-596",
                "",
                "--- scanner 4 ---",
                "727,592,562",
                "-293,-554,779",
                "441,611,-461",
                "-714,465,-776",
                "-743,427,-804",
                "-660,-479,-426",
                "832,-632,460",
                "927,-485,-438",
                "408,393,-506",
                "466,436,-512",
                "110,16,151",
                "-258,-428,682",
                "-393,719,612",
                "-211,-452,876",
                "808,-476,-593",
                "-575,615,604",
                "-485,667,467",
                "-680,325,-822",
                "-627,-443,-432",
                "872,-547,-609",
                "833,512,582",
                "807,604,487",
                "839,-516,451",
                "891,-625,532",
                "-652,-548,-490",
                "30,-46,-14"
                ],
            testResult=Just "79",
            testResult2=Just "3621"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse :: String -> [Set [Int]]
parse = p . lines
  where
    p [] = []
    p (_:items) = fromList (map parseInts beacons) : p (drop 1 rest)
      where (beacons,rest) = span (not . null) items

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (a:as) = map ((,) a) as ++ pairs as

canonicalDiff :: (Num a, Ord a) => ([a],[a]) -> [a]
canonicalDiff (a,b) = sort $ map abs $ zipWith (-) a b

canonicalDiffs :: Set [Int] -> Map [Int] [([Int],[Int])]
canonicalDiffs = foldr collect empty . pairs . elems
  where
    collect pair = alter (Just . maybe [pair] (pair:)) (canonicalDiff pair)

connect :: Set [Int] -> Map [Int] [([Int],[Int])] -> [Set [Int]] -> [(Set [Int],Map [Int] [([Int],[Int])])] -> [(Set [Int],Map [Int] [([Int],[Int])])] -> [([Int],Set [Int])]
connect pivot pcdiffs pivots [] [] = []
connect _ _ (pivot:pivots) [] unconnected =
    connect pivot (canonicalDiffs pivot) pivots unconnected []
connect pivot pcdiffs pivots (scanner@(beacons,cdiffs):scanners) unconnected
  | commonCdiffs < 66 =
      connect pivot pcdiffs pivots scanners (scanner:unconnected)
  | otherwise =
      (offset,remappedBeacons) : connect pivot pcdiffs (remappedBeacons:pivots)
                                         scanners unconnected
  where
    commonCdiffs =
        sum $ mergeWithKey merge (const empty) (const empty) pcdiffs cdiffs
      where
        merge _ a b = Just $ min (length a) (length b)
    ((offset,remappedBeacons):_) =
        [(offs,rmBeacons)
         | (pcdiff,ppairs) <- toList pcdiffs,
           (pbeacon1,pbeacon2) <- ppairs,
           (beacon1,beacon2) <- findWithDefault [] pcdiff cdiffs,
           (b1,b2) <- [(beacon1,beacon2),(beacon2,beacon1)],
           rotate <- rotations,
           (ob1,ob2) <- [(rotate b1,rotate b2)],
           diff ob1 ob2 == diff pbeacon1 pbeacon2,
           offs <- [diff pbeacon1 ob1],
           rmBeacons <- [Data.Set.map (remap rotate offs) beacons],
           size (intersection rmBeacons pivot) >= 12]

rotations :: [[Int] -> [Int]]
rotations =
    [\ [x,y,z] -> [x,y,z],     \ [x,y,z] -> [x,z,-y],
     \ [x,y,z] -> [x,-y,-z],   \ [x,y,z] -> [x,-z,y],
     \ [x,y,z] -> [-x,y,-z],   \ [x,y,z] -> [-x,-z,-y],
     \ [x,y,z] -> [-x,-y,z],   \ [x,y,z] -> [-x,z,y],
     \ [x,y,z] -> [y,-x,z],    \ [x,y,z] -> [y,z,x],
     \ [x,y,z] -> [y,x,-z],    \ [x,y,z] -> [y,-z,-x],
     \ [x,y,z] -> [-y,-x,-z],  \ [x,y,z] -> [-y,-z,x],
     \ [x,y,z] -> [-y,x,z],    \ [x,y,z] -> [-y,z,-x],
     \ [x,y,z] -> [z,y,-x],    \ [x,y,z] -> [z,-x,-y],
     \ [x,y,z] -> [z,-y,x],    \ [x,y,z] -> [z,x,y],
     \ [x,y,z] -> [-z,y,x],    \ [x,y,z] -> [-z,x,-y],
     \ [x,y,z] -> [-z,-y,-x],  \ [x,y,z] -> [-z,-x,y]]

remap :: ([Int] -> [Int]) -> [Int] -> [Int] -> [Int]
remap rotate translate = zipWith (+) translate . rotate

diff :: [Int] -> [Int] -> [Int]
diff a b = zipWith (-) a b

mapScanners :: [Set [Int]] -> [([Int],Set [Int])]
mapScanners (pivot:scanners) =
    ([0,0,0],pivot) : connect pivot pcdiffs [] scannersWithCDiffs []
  where
    pcdiffs = canonicalDiffs pivot
    scannersWithCDiffs = zip scanners $ map canonicalDiffs scanners

result :: [Set [Int]] -> Int
result = size . unions . map snd . mapScanners

mdist :: ([Int],[Int]) -> Int
mdist (a,b) = sum $ map abs $ zipWith (-) a b

result2 :: [Set [Int]] -> Int
result2 = maximum . map mdist . pairs . map fst . mapScanners
