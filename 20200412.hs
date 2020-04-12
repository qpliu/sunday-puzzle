import qualified Data.Map as M
import qualified Data.Set as S

valid :: String -> String -> Bool
valid [a1,a2,a3,a4,a5] [b1,b2,b3,b4,b5] =
    (a1 /= b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5) ||
    (a1 == b1 && a2 /= b2 && a3 == b3 && a4 == b4 && a5 == b5) ||
    (a1 == b1 && a2 == b2 && a3 /= b3 && a4 == b4 && a5 == b5) ||
    (a1 == b1 && a2 == b2 && a3 == b3 && a4 /= b4 && a5 == b5) ||
    (a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 /= b5)
valid _ _ = False

step :: S.Set String -> M.Map String [String] -> (S.Set String,M.Map String [String])
step dict ladders = (S.difference dict (M.keysSet newLadders),newLadders)
  where
    newLadders = M.fromList (concatMap findSteps (M.elems ladders))
    findSteps ladder@(word:_) =
        [(next,next:ladder) | next <- S.elems dict, valid next word]

ladders :: M.Map String [String] -> M.Map String [String] -> [[String]]
ladders fromTop fromBottom = M.elems (M.intersectionWith join fromTop fromBottom)
  where join top bottom = reverse top ++ drop 1 bottom

find :: String -> String -> S.Set String -> [[String]]
find top bottom dict =
    search (S.delete top dict,M.fromList [(top,[top])]) (S.delete bottom dict,M.fromList [(bottom,[bottom])])
  where
    search (topDict,fromTop) (bottomDict,fromBottom)
      | null fromTop || null fromBottom = []
      | not (null joinNewTop) = joinNewTop
      | not (null joinNewBottom) = joinNewBottom
      | otherwise = search (newTopDict,newFromTop) (newBottomDict,newFromBottom)
      where
        (newTopDict,newFromTop) = step topDict fromTop
        (newBottomDict,newFromBottom) = step bottomDict fromBottom
        joinNewTop = ladders newFromTop fromBottom
        joinNewBottom = ladders newFromTop newFromBottom

weird :: String -> Bool
weird word = S.member word (S.fromList [
  "larve","varve","seave","marge","merle","ceile","parge","perse","peise",
  "seise","spise","porgy","porry","targe","tarse","scuse","scase","barie",
  "baria","barra","serry","seary","barse","garse","pargo","parto","sorra",
  "scall","whill","whilk","bange","bande","bange","mease","mense","whase",
  "gange","gunge","guige","bargh","borgh","rougy","roupy","shaly","shapy",
  "teave","thave","barth","baith","writh","bodge","bouge","bouse","boose",
  "blart","slart","smalt","gadge","gaize","gaine","gwine","swile","cardo",
  "bardy","blady","slade","bolly","boily","bulse","buist","suist","suint",
  "suant","sargo","sorgo","foute","sairy","spirt","lairy","soary","stary",
  "padge","padle","parle","parly","warly","waily","saily","fordy","busky",
  "booky","sooky","spole","caggy","caber","corke","fodge","fadge","forme",
  "spole","spale","shawy","socky","spall","chare","turse","sorty","sonly",
  "songy","smily","speer","shiel","druse","drupe","crape","chape","shide",
  "shole","porta","coroa","carga","soral","seral","sough","torse","warse",
  "warst","whift","seraw","stell","scrat","surat","spier","purre","platy",
  "slaty","coapt","chapt","rudge","rodge","routh","slote","shirl","skeer",
  "skeel","skell","sheal","braky","brank","beant","phare","cuddy","denty",
  "foaly","guily","stime","stimy","sence","gumly","whone","slive","slare",
  "seity","saimy","malmy","birse","biose","brose","phose","cully","colly",
  "porge","formy","flamy","flane","slane","stane","curst","durst","darst",
  "darat","daraf","saraf","shraf","cruse","cruce","cruck","slash","cerin",
  "stech","stich","belah","benny","choky","sweer","swelt","swelp","speel",
  "chaft","trave","trady","sprag","sweal","speal","warve","salay","shaul",
  "shier"])

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . find "large" "small" . S.fromList . filter (not . weird) . filter ((== 5) . length) . lines

-- ["large","barge","badge","budge","pudge","purge","puree","purer","parer","payer","sayer","shyer","sheer","shear","sheaf","shelf","shell","smell","small"]
