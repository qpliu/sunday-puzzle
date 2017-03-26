import Data.List(sort)
import Data.Map(Map,alter,empty,lookup,member)
import Prelude hiding (lookup)

collectfks :: [String] -> [String] -> Map String [(String,String)]
collectfks fs ks =
    foldl collect empty [(sort (f++k),(f,k)) | f <- fs, k <- ks, elem 'g' (f++k)]
  where
    collect fks (k,v) = alter (Just . maybe [v] (v:)) k fks

collectgs :: [String] -> [String] -> Map String [(String,String)] -> [(String,String,Maybe [(String,String)])]
collectgs gs ks fks =
    filter possible [(g,k,lookup (sort (g++k)) fks) | g <- gs, k <- ks]
  where
    possible (_,_,Nothing) = False
    possible _ = True

collectfgks :: [String] -> ([String],[String],[String])
collectfgks words = foldl collect ([],[],[]) words
  where
    collect (fs,gs,ks) w | take 1 w == "f" = (w:fs,gs,ks)
                         | take 1 w == "g" = (fs,w:gs,ks)
                         | take 1 w == "k" = (fs,gs,w:ks)
                         | otherwise = (fs,gs,ks)

main :: IO ()
main = do
    (fs,gs,ks) <- fmap (collectfgks . lines) (readFile "/usr/share/dict/words")
    let gs = ["guacamole","groundbeef","groundpepper","groceries","greens","greenbeans","greenonions","granola","griddle"]
    let fs = ["fryingpan","fridge"]
    mapM_ print (collectgs gs ks (collectfks fs ks))

-- griddle,knife,fridge,kindle
