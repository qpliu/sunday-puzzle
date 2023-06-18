import Data.List(sort)
import Data.Map(Map,fromList,member,(!))

fish :: [String]
fish = ["eel","tuna","trout","shark","moray","gar","ahi","carp","perch","koi",
    "tetra","goldfish","sardine","catfish","ray","manta","salmon","coho",
    "remora","stingray","hammerhead","mako"]

mammals :: [String]
mammals = ["ape","horse","monkey","cat","dog","sheep","goat","elk","yak",
    "human","man","koala","bear","puma","lion","tiger","lamb","sloth",
    "lemur","panda","pig","boar","ram","stag","deer","cow","cattle","bison",
    "leopard","cheetah","giraffe","rhino","rhinoceros","zebra","gnu",
    "anteater","tapir","ox","bull","gibbon","mare","pony","buffalo","skunk",
    "squirrel","rat","bat","mouse","mole","vole","hog","hamster","gerbil",
    "chipmunk","stag","ass","donkey","armadillo","opossum","moose","reindeer",
    "racoon","kitten","puppy","fox","wolf","kangaroo","coyote","lynx","stoat",
    "weasel","sable","mink"]

reptiles :: Map String String
reptiles = fromList [(sort item,item) | item <- [
    "lizard","alligator","crocodile","dinosaur","tortoise",
    "turtle","iguana","gilamonster","anaconda","boaconstrictor","gecko",
    "seaturtle","snappingturtle","boxtortoise","skink","snake","viper",
    "asp","pitviper","cobra","kingcobra","gartersnake","coralsnake",
    "diamondback","diamondhead","raptor","velociraptor","triceratops",
    "tyrannosaur","plesiosaur","pterodactyl"]]

main :: IO ()
main = mapM_ print [(m,f,reptiles!(sort (m++f))) | f <- fish, m <- mammals, sort (m++f) `member` reptiles]

-- tiger salmon gila monster
