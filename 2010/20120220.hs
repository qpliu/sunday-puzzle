import Data.Map(Map,(!),fromList,keys)
import Data.Maybe(catMaybes,listToMaybe)

states :: Map String [String]
states = fromList
  [("alabama",["mississippi","tennessee","georgia","florida"]),
   ("arizona",["california","nevada","utah","newmexico"]),
   ("newmexico",["arizona","colorado","oklahoma","texas"]),
   ("arkansas",["oklahoma","missouri","tennessee","mississippi","louisiana","texas"]),
   ("louisiana",["texas","arkansas","mississippi"]),
   ("mississippi",["arkansas","tennessee","alabama","louisiana"]),
   ("california",["oregon","nevada","arizona"]),
   ("nevada",["oregon","idaho","utah","arizona","california"]),
   ("massachusetts",["newyork","vermont","newhampshire","rhodeisland","connecticut"]),
   ("rhodeisland",["massachusetts","connecticut"]),
   ("connecticut",["newyork","massachusetts","rhodeisland"]),
   ("colorado",["wyoming","nebraska","kansas","oklahoma","newmexico","utah"]),
   ("florida",["georgia","alabama"]),
   ("georgia",["tennessee","northcarolina","southcarolina","florida","alabama"]),
   ("idaho",["washington","montana","wyoming","utah","nevada","oregon"]),
   ("illinois",["wisconsin","indiana","kentucky","missouri","iowa"]),
   ("indiana",["illinois","michigan","ohio","kentucky"]),
   ("iowa",["minnesota","wisconsin","illinois","missouri","nebraska","southdakota"]),
   ("kansas",["nebraska","missouri","oklahoma","colorado"]),
   ("kentucky",["illinois","indiana","ohio","westvirginia","virginia","tennessee","missouri"]),
   ("tennessee",["kentucky","virginia","northcarolina","georgia","alabama","mississippi","arkansas","missouri"]),
   ("westvirginia",["ohio","pennsylvania","maryland","virginia","kentucky"]),
   ("maryland",["westvirginia","pennsylvania","delaware","virginia"]),
   ("delaware",["pennsylvania","newjersey","maryland"]),
   ("virginia",["westvirginia","maryland","northcarolina","tennessee","kentucky"]),
   ("maine",["newhampshire"]),
   ("michigan",["wisconsin","ohio","indiana"]),
   ("minnesota",["northdakota","wisconsin","iowa","southdakota"]),
   ("missouri",["iowa","illinois","kentucky","tennessee","arkansas","oklahoma","kansas","nebraska"]),
   ("montana",["idaho","northdakota","southdakota","wyoming"]),
   ("nebraska",["wyoming","southdakota","iowa","missouri","kansas","colorado"]),
   ("newhampshire",["maine","massachusetts","vermont"]),
   ("vermont",["newyork","newhampshire","massachusetts"]),
   ("newjersey",["pennsylvania","newyork","delaware"]),
   ("newyork",["pennsylvania","vermont","massachusetts","connecticut","newjersey"]),
   ("northcarolina",["tennessee","virginia","southcarolina","georgia"]),
   ("southcarolina",["northcarolina","georgia"]),
   ("northdakota",["montana","minnesota","southdakota"]),
   ("oklahoma",["colorado","kansas","missouri","arkansas","texas","newmexico"]),
   ("ohio",["michigan","pennsylvania","westvirginia","kentucky","indiana"]),
   ("pennsylvania",["newyork","newjersey","delaware","maryland","westvirginia","ohio"]),
   ("oregon",["washington","idaho","nevada","california"]),
   ("southdakota",["montana","northdakota","minnesota","iowa","nebraska","wyoming"]),
   ("texas",["newmexico","oklahoma","arkansas","louisiana"]),
   ("utah",["idaho","wyoming","colorado","arizona","nevada"]),
   ("washington",["idaho","oregon"]),
   ("wisconsin",["minnesota","michigan","illinois","iowa"]),
   ("wyoming",["montana","southdakota","nebraska","colorado","utah","idaho"])]

check :: String -> Bool
check state = all check' (states ! state)
  where check' state' = state `elem` (states ! state')

condense :: (a -> Maybe b) -> [a] -> Maybe b
condense f = listToMaybe . take 1 . catMaybes . map f

findrt :: String -> Maybe ((Int,String),[String])
findrt word = fmap ((,) (length word,word) . reverse) $ condense (findrt' word []) $ keys states

findrt' :: String -> [String] -> String -> Maybe [String]
findrt' word accumulator state
{-| state `elem` accumulator = Nothing-}
  | otherwise = findrt'' word accumulator state state

findrt'' :: String -> [String] -> String -> String -> Maybe [String]
findrt'' word accumulator state match
  | null word = Just (state:accumulator)
  | null match = condense (findrt' word (state:accumulator)) (states ! state)
  | head word /= head match = Nothing
  | otherwise = maybe (condense (findrt' (tail word) (state:accumulator)) (states ! state)) Just (findrt'' (tail word) accumulator state (tail match))

main :: IO ()
main = readFile "/usr/share/dict/words" >>= sequence_ . map (putStrLn . show) . catMaybes . map findrt . filter ((>= 8) . length) . lines
