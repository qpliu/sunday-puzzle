import Control.Monad(foldM_)
import Data.List(sort)
import Data.Map(Map,insert,lookup,empty)
import Prelude hiding (lookup)

add7 :: Map String ([String],[String]) -> String -> (Maybe [String],Map String ([String],[String]))
add7 dict word =
    case word of
      ('a':'y':t) -> add7' (sort t)
      (h:'a':'y':t) -> add7' (sort (h:t))
      (h1:h2:'a':'y':t) -> add7' (sort (h1:h2:t))
      (h1:h2:h3:'a':'y':t) -> add7' (sort (h1:h2:h3:t))
      (h1:h2:h3:h4:'a':'y':t) -> add7' (sort (h1:h2:h3:h4:t))
      (h1:h2:h3:h4:h5:'a':'y':t) -> add7' (sort (h1:h2:h3:h4:h5:t))
      _ -> (Nothing,dict)
  where
    add7' k =
        case lookup k dict of
          Nothing -> (Nothing,insert k ([],[word]) dict)
          Just ([],words) -> (Nothing,insert k ([],word:words) dict)
          Just (words5,words) -> (Just words5,insert k (words5,word:words) dict)

add5 :: Map String ([String],[String]) -> String -> (Maybe [String],Map String ([String],[String]))
add5 dict word =
    let k = sort word
    in  case lookup k dict of
          Nothing -> (Nothing,insert k ([word],[]) dict)
          Just (words,[]) -> (Nothing,insert k (word:words,[]) dict)
          Just (words,words7) -> (Just words7,insert k (word:words,words7) dict)

add :: Map String ([String],[String]) -> String -> (Maybe [String],Map String ([String],[String]))
add dict word
  | length word == 5 = add5 dict word
  | length word == 7 = add7 dict word
  | otherwise = (Nothing,dict)

collect :: Map String ([String],[String]) -> String -> IO (Map String ([String],[String]))
collect dict word =
  let (match,dict') = add dict word
  in  maybe (return ()) (print . (,) word) match >> return dict'

main :: IO ()
main = readFile "/usr/share/dict/words" >>= foldM_ collect empty . lines

-- dream daymare
