import Data.List(sort)
import Data.Map(Map,empty,insert,lookup,toList)
import Prelude hiding (lookup)

gather :: [String] -> Map String String
gather items = foldl add empty items
  where add dict item = insert (sort item) item dict

search :: Map String String -> [Maybe ((String,String),String)]
search dict = [maybe Nothing (Just . (,) (v1,v2)) (lookup (sort (k1++k2)) dict) | (k1,v1) <- toList dict, (k2,v2) <- toList dict]

main :: IO ()
main = mapM_ (maybe (return ()) print) $ search $ gather [
 "dog", "cat", "horse", "cow", "pig", "hog", "elephant", "camel",
 "hippopotamus", "gnu", "elk", "deer", "sheep", "goat", "orangutan",
 "kangaroo", "platypus", "lemur", "gorilla", "ape", "mandrill", "tiger",
 "zebra", "lion", "ocelot", "puma", "monkey", "rhinoceros", "whale", "orca",
 "sealion", "otter", "seaotter", "seal", "reindeer", "buffalo", "bison",
 "yak", "leopard", "vole", "mole", "chipmunk", "squirrel", "raccoon", "marmot",
 "weasel", "rat", "bat", "mouse", "rabbit", "hare", "dolphin", "porpoise",
 "human", "alpaca", "llama", "burro", "ass", "donkey", "bear", "koala",
 "panda", "woodchuck", "beaver", "skunk", "hamster", "giraffe", "cougar",
 "eland", "gibbon", "antelope", "ox", "fox", "wolf", "moose", "cheetah",
 "aardvark", "gazelle", "mongoose", "porcupine", "gerbil", "shrew", "baboon",
 "hyena", "wildebeest", "dromedary", "oryx", "hedgehog", "impala", "jackal",
 "polecat", "meerkat", "kudu", "pangolin", "okapi", "macaque", "badger",
 "lynx", "marten", "muskrat", "civet", "marmoset", "ibex", "stoat", "walrus",
 "wolverine", "mink", "bobcat", "gopher", "jaguar", "vervet", "opossum",
 "mule", "coyote", "armadillo", "sloth", "capuchin", "agouti", "anteater",
 "tapir", "chinchilla", "titi", "manatee", "bushbaby", "guineapig",
 "duiker", "gundi", "coypu", "dikdik", "rhebok", "mangabey", "sengi",
 "dipodidae", "genet", "kusimanse", "dugong"
 ]

-- dog gnu dugong
