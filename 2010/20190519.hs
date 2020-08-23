import Data.Char(toLower)

has :: String -> String -> Bool
has _ [] = True
has [] _ = False
has (p:ps) (c:cs)
  | p == c = has ps cs
  | otherwise = has ps (c:cs)

test :: String -> [(String,String)]
test profession
  | length profession /= 13 = []
  | otherwise =
        [(profession,country) | (country,c) <- countries, profession `has` c]

main :: IO ()
main =
    readFile "/usr/share/dict/words" >>= mapM_ print . concatMap test . lines

countries :: [(String,String)]
countries = [(c,map toLower c) | c <- [ "Ghana", "Samoa", "China", "Egypt", 
    "Syria", "Qatar", "Nepal", "Sudan", "Gabon", "Spain", "Kenya", "Burma",
    "India", "Palau", "Niger", "Tonga", "Aruba", "Haiti", "Benin", "Korea",
    "Italy", "Yemen", "Chile", "Japan", "Libya", "Malta", "Congo"]]

-- hieroglyphist Egypt
