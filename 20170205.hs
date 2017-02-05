import Data.Char(isAlpha,toUpper)
import Data.List(sort)
import Data.Maybe(catMaybes)

names :: [String]
names = [
  "Cinderella", "Prince Charming", "The Beast", "Beast", "Beauty",
  "Sleeping Beauty", "Snow White", "Evil Stepmother", "Little Red Riding Hood",
  "The Wolf", "The Big Bad Wolf", "The Fairy Godmother", "Fairy Godmother",
  "Tinker Bell", "Peter Pan", "Ali Baba", "The Genie", "Genie",
  "Evil stepsister", "Evil stepsisters", "The Woodsman", "Woodsman",
  "Mother Goose", "Father Time", "The Wizard Of Oz", "Wizard Of Oz", "Dorothy",
  "Toto", "The Cowardly Lion", "Cowardly Lion", "The Tin Woodman",
  "Tin Woodman", "The Scarecrow", "Scarecrow", "The Wicked Witch of the East",
  "Wicked Witch of the East", "Glenda the Good Witch", "Hans Brinker",
  "Peter Piper", "The Little Mermaid", "Little Mermaid", "Hansel", "Gretel",
  "Aladdin", "The Seven Dwarves", "Captain Hook", "Smee", "Puss in Boots",
  "The Emperor", "Emperor", "The Frog Prince", "Frog Prince", "Goldilocks",
  "The Little Match Girl", "Little Match Girl", "Pinocchio", "Jiminy Cricket",
  "Jack", "The Giant", "Giant", "Rapunzel", "Rumpelstiltskin",
  "The Little Red Hen", "Little Red Hen", "The Pied Piper", "Pied Piper",
  "Sinbad", "Sinbad the Sailor", "The Snow Queen", "Snow Queen",
  "The Billy Goats Gruff", "Billy Goats Gruff", "Thumbelina", "Tom Thumb",
  "The Ugly Duckling", "Ugly Duckling", "The Ice Maiden", "Ice Maiden",
  "The Sea King", "Sea King", "The Wicked Witch", "Wicked Witch",
  "The Witch", "Witch", "Shrek", "Donkey", "Lord Farquaad", "Gingerbread Man",
  "Princess Fiona", "Dragon", "Robin Hood", "Geppetto", "Papa Bear",
  "Mama Bear", "Baby Bear", "Babar", "Dumbo", "Tiny Tim", "Ebenezer Scrooge",
  "Alice", "Alice Liddel", "The Mad Hatter", "Mad Hatter", "The March Hare",
  "March Hare", "The Queen of Hearts", "Queen of Hearts", "Tweedledum",
  "Tweedledee", "The Cheshire Cat", "Cheshire Cat", "The Prince", "Prince",
  "The King", "King", "Old King Cole", "The Queen", "Queen", "Charlie Bucket",
  "Charlie", "Willie Wonka", "Grandpa Joe", "Veruca Salt", "Mike Teevee",
  "Violet Beauregard", "Aslan", "Briar Rose", "Queen Briar Rose",
  "Princess Aurora", "Aurora", "Rip Van Winkle", "Hans im Gluck",
  "Hans im Glueck", "Hans in Luck", "Hans in Luck", "The Frog Princess",
  "Frog Princess", "The Pied Piper Of Hamelin", "Pied Piper Of Hamelin",
  "Red Riding Hood", "The Woodcutter", "Woodcutter", "Mother Hubbard",
  "Old Mother Hubbard", "King Thrushbeard", "Buttercup", "Westley",
  "Dread Pirate Roberts", "Prince Humperdinck", "Humperdinck", "Inigo Montoya",
  "The Princess Bride", "Princess Bride", "Bluebeard", "King Midas", "Midas",
  "Humpty Dumpty", "Icarus", "Daedalus", "The Minotaur", "Minotaur",
  "Jack Horner", "Jack Frost", "Little Jack Horner", "Jack O'Lantern",
  "Jack the Giant Killer", "Ballerina", "The Tin Soldier", "Tin Soldier",
  "Ariel", "Flounder", "King Triton", "Triton", "Sebastian", "Ursula",
  "Doc", "Grumpy", "Happy", "Sleepy", "Bashful", "Sneezy", "Dopey",
  "Evil Queen Grimhilde", "The Magic Mirror", "Magic Mirror",
  "Humbert the Huntsman", "Humbert", "Jafar", "Abu", "Princess Jasmine",
  "Jasmine", "Iago", "Abu", "Gaston", "Lumiere", "Cogsworth", "Mrs Potts",
  "Chip", "Maurice", "Lefou", "Phillipe", "The Wardrobe", "Wardrobe",
  "Simba", "Mufasa", "Scar", "Timon", "Pumbaa", "Nala", "Rafiki", "Zazu",
  "Maleficent", "Flora", "Fauna", "Merryweather", "Rose", "Prince Philip",
  "Philip", "King Stefan", "Stefan", "King Hubert", "Hubert", "Queen Leah",
  "Leah", "Anastasia Tremaine", "Anastasia", "Lady Tremaine",
  "The Wicked Stepmother", "Wicked Stepmother", "Drizella Tremain", "Drizella",
  "The White Rabbit", "White Rabbit", "Dick Whittington", "Gulliver",
  "The Marquis of Carabas", "Marquis of Carabas", "The Marquis", "Marquis",
  "Jack Spriggins", "Uncle Remus", "Remus", "Brer Rabbit", "Scheherazade",
  "Prince Hyacinth", "Hyacinth", "The Sphinx", "Sphinx", "Perseus", "Medusa",
  "Morgiana", "Rose Red", "Falada", "Conrad", "Falkor", "Atreyu"
  ]

answers :: [(String)] -> [(String,String,Char,Char)]
answers names =
  catMaybes [match item1 item2 |
             item1 <- items names, item2 <- items names, item1 > item2]

items :: [(String)] -> [(String,String)]
items names = zip names (map (sort . map toUpper . filter isAlpha) names)

match :: (String,String) -> (String,String) -> Maybe (String,String,Char,Char)
match (name1,key1) (name2,key2) = m key1 key2 Nothing Nothing
  where
    m [] [] (Just c1) (Just c2) = Just (name1,name2,c1,c2)
    m [] [c2] (Just c1) Nothing = Just (name1,name2,c1,c2)
    m [c1] [] Nothing (Just c2) = Just (name1,name2,c1,c2)
    m [] _ _ _ = Nothing
    m _ [] _ _ = Nothing
    m key1@(k1:k1s) key2@(k2:k2s) Nothing Nothing
      | k1 == k2 = m k1s k2s Nothing Nothing
      | otherwise = maybe (m key1 k2s Nothing (Just k2))
                     Just (m k1s key2 (Just k1) Nothing)
    m key1@(k1:k1s) (k2:k2s) c1@(Just _) Nothing
      | k1 == k2 = m k1s k2s c1 Nothing
      | otherwise = m key1 k2s c1 (Just k2)
    m (k1:k1s) key2@(k2:k2s) Nothing c2@(Just _)
      | k1 == k2 = m k1s k2s Nothing c2
      | otherwise = m k1s key2 (Just k1) c2
    m (k1:k1s) (k2:k2s) c1@(Just _) c2@(Just _)
      | k1 == k2 = m k1s k2s c1 c2
      | otherwise = Nothing

main :: IO ()
main = mapM_ print (answers names)

-- CONRAD C -> G DRAGON
-- ATREYU R -> B BEAUTY
