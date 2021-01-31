import Data.List(nub,sort)
import Data.Map(Map,fromList,keys,(!))
import Data.Set(Set,member)
import qualified Data.Set

paths :: Ord a => Map a [a] -> Int -> a -> [[a]]
paths graph depth start
  | depth > 0 = map (start:) (concatMap (paths graph (depth-1)) (graph!start))
  | otherwise = [[]]

candidates :: Map String [String] -> Int -> Set String
candidates graph depth = Data.Set.fromList $ concat $ map (nub . map (sort . concat) . paths graph depth) (keys graph)

findPaths :: String -> Map String [String] -> Int -> [[String]]
findPaths word graph depth = filter ((sort word==) . (sort . concat)) $ concatMap (paths graph depth) (keys graph)

main :: IO ()
main = readFile "/usr/share/dict/words" >>= print . map addPath . filter (flip member set . sort) . lines
  where
    depth = 4
    set = candidates usa depth
    addPath word = (word,take 1 (findPaths word usa depth))

-- animator: ar tn mo ia
-- condense: co ne sd ne
-- condemns: co ne sd mn
-- diamonds: mo ia sd nd
-- dioramas: ar mo ia sd
-- flagrant: ar tn ga fl
-- moleskin: il mo ne ks
-- moralism: il mo ar ms
-- motorman: ar mo tn mo
-- nomadism: mn sd ia mo
-- nominate: ia ne mo tn
-- nonmetal: al tn mo ne
-- ornament: ar tn mo ne
-- tankroom: ar ok mo tn
-- unsocket: ks ne co ut

-- 10-letter words:
-- alkalinity: al tn ky il ia
-- immolation: ia il mo tn mo
-- ornamental: al tn ar mo ne

-- 6-letter words:
-- amidst: ia sd mt
-- amoral: la ar mo
-- cutout: ut co ut
-- damped: de md pa
-- enamor: ar mo ne
-- matron: ar mo tn
-- monkey: ky mo ne
-- second: co ne sd

usa :: Map String [String]
usa = fromList [
    ("wa",["or","id"]),
    ("or",["wa","id","ca","nv"]),
    ("ca",["or","nv","az"]),
    ("nv",["ca","az","ut","or","id"]),
    ("id",["wa","or","mt","wy","ut","nv"]),
    ("ut",["nv","id","wy","co","az"]),
    ("az",["ca","nv","ut","nm"]),
    ("mt",["id","nd","sd","wy"]),
    ("wy",["id","mt","sd","ne","co","ut"]),
    ("co",["wy","ne","ks","ok","nm","ut"]),
    ("nm",["az","co","ok","tx"]),
    ("nd",["mt","mn","sd"]),
    ("sd",["mt","nd","mn","ia","ne","wy"]),
    ("ne",["wy","sd","ia","mo","ks","co"]),
    ("ks",["co","ne","mo","ok"]),
    ("ok",["nm","co","ks","mo","ar","tx"]),
    ("tx",["nm","ok","ar","la"]),
    ("mn",["nd","wi","ia","sd"]),
    ("ia",["sd","mn","wi","il","mo","ne"]),
    ("mo",["ne","ia","il","ky","tn","ar","ok","ks"]),
    ("ar",["ok","mo","tn","ms","la","tx"]),
    ("la",["tx","ar","ms"]),
    ("ky",["mo","il","in","oh","wv","va","tn"]),
    ("tn",["mo","ky","va","nc","ga","al","ms","ar"]),
    ("ms",["ar","tn","al","la"]),
    ("al",["ms","tn","ga","fl"]),
    ("fl",["al","ga"]),
    ("ga",["al","tn","nc","sc","fl"]),
    ("sc",["nc","ga"]),
    ("nc",["tn","va","sc"]),
    ("va",["ky","wv","md","nc","tn","dc"]),
    ("wv",["oh","pa","md","va","ky"]),
    ("wi",["mn","mi","il","ia"]),
    ("mi",["wi","oh","in"]),
    ("il",["ia","wi","in","ky","mo"]),
    ("in",["il","mi","oh","ky"]),
    ("oh",["in","mi","pa","wv","ky"]),
    ("pa",["oh","ny","nj","de","md","wv"]),
    ("md",["wv","pa","de","va","dc"]),
    ("de",["md","pa","nj"]),
    ("nj",["pa","ny","de"]),
    ("ny",["vt","ma","ct","nj","pa"]),
    ("ct",["ny","ma","ri"]),
    ("ri",["ct","ma"]),
    ("ma",["ny","vt","nh","ri","ct"]),
    ("vt",["ny","nh","ma"]),
    ("nh",["vt","me","ma"]),
    ("me",["nh"]),
    ("dc",["va","md"])
    ]
