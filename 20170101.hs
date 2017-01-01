import Data.Map(Map)
import qualified Data.Map as M

index :: Map String [String]
index = foldl add M.empty names
  where
    add m name = foldl (addPrefix name) m [1..4]
    addPrefix name m n =
        M.alter (Just . maybe [name] (++[name])) (take n name) m

key :: [String] -> String
key heads = map (head . drop (length heads)) heads

answers :: [(String,String,String,String,String)]
answers = [(n1,n2,n3,n4,n5) |
            n1 <- names,
            n2 <- maybe [] id (M.lookup (key [n1]) index),
            n3 <- maybe [] id (M.lookup (key [n1,n2]) index),
            n4 <- maybe [] id (M.lookup (key [n1,n2,n3]) index),
            n5 <- maybe [] id (M.lookup (key [n1,n2,n3,n4]) index)]

names :: [String]
names = ["JACOB", "ETHAN", "AIDEN", "MASON", "DAVID", "LOGAN", "JAMES",
         "DYLAN", "CALEB", "TYLER", "LUCAS", "GAVIN", "ISAAC", "ANGEL",
         "AARON", "WYATT", "KEVIN", "CHASE", "HENRY", "JASON", "AYDEN",
         "BRODY", "DIEGO", "BLAKE", "JADEN", "JESUS", "AIDAN", "BRYAN",
         "JAXON", "BRIAN", "NOLAN", "RILEY", "KADEN", "MICAH", "COLIN",
         "BRYCE", "KALEB", "CADEN", "JESSE", "BRADY", "RYDER", "MILES",
         "ASHER", "DEVIN", "ELIAS", "JONAH", "OSCAR", "GRANT", "JORGE",
         "DEREK", "RYLAN", "JOSUE", "ROMAN", "PETER", "ERICK", "DRAKE",
         "SHANE", "CESAR", "MARIO", "EDWIN", "AVERY", "EDGAR", "MATEO",
         "SILAS", "ANDRE", "LUKAS", "TYSON", "SHAWN", "JARED", "SIMON",
         "MYLES", "DANTE", "MARCO", "KYLER", "QUINN", "COLBY", "PEDRO",
         "MALIK", "RUBEN", "JUDAH", "JAYCE", "JAKOB", "FRANK", "BROCK",
         "RYKER", "AMARI", "COREY", "DANNY", "ALLEN", "ROWAN", "FELIX",
         "JULIO", "LOUIS", "TRENT", "KEITH", "RANDY", "SCOTT", "ROMEO",
         "COHEN", "PABLO", "JAIME", "LARRY", "JIMMY", "JERRY", "BRETT",
         "ROCCO", "JALEN", "CHRIS", "TITUS", "CASEY", "DAMON", "DAVIS",
         "REECE", "RIVER", "ISSAC", "YAHIR", "URIEL", "LANCE", "RICKY",
         "JOHAN", "JONAS", "TALON", "BYRON", "ORION", "CASON", "RAMON",
         "KASON", "MEKHI", "SHAUN", "CYRUS", "REESE", "TERRY", "AMARE",
         "RONAN", "EDDIE", "MOSES", "ROGER", "NASIR", "AHMAD", "TRACE",
         "ALVIN", "CONOR", "ABRAM", "BRUCE", "ROHAN", "AHMED", "URIAH",
         "BOBBY", "AADEN", "KASEN", "JAVON", "AARAV", "JADON", "MOSHE",
         "TOMMY", "ISIAH", "ARIEL", "RHETT", "LAYNE", "ALLAN", "JAMAL",
         "BILLY", "KENNY", "TOMAS", "AYDAN", "ARJUN", "CHACE", "OMARI",
         "BRENT", "HARRY", "AYDIN", "CRAIG", "TRIPP", "KYLAN", "SEMAJ",
         "KOLBY", "MAKAI", "DAVIN", "CLARK", "JAMIE", "JAIRO", "JAMIR",
         "AIDYN", "LAMAR", "ARYAN", "YUSUF", "GAVYN", "LEWIS", "ZAYNE",
         "RYLEE", "SOREN", "MAXIM", "ELLIS", "EMERY", "ROYCE", "TATUM",
         "BRUNO", "HAMZA", "AYAAN", "RAYAN", "HEATH", "VANCE", "WAYNE",
         "STEVE", "ELIAN", "JAMAR", "YOSEF", "SAMIR", "LYRIC", "DEVEN",
         "KYSON", "DILAN", "KYRON", "SYLAS", "JARON", "ALDEN", "BLAZE",
         "KRISH", "RONIN", "BRICE", "TYREE", "DEVAN", "CASEN", "MAJOR",
         "GAIGE", "AEDAN", "JAXEN", "JADYN", "KADYN", "AMEER", "DAVON",
         "ZAIRE", "BODHI", "DONTE", "ARNAV", "DEVYN", "HAYES", "NIGEL",
         "DARIO", "LEROY", "CHAIM", "SONNY", "ELIOT", "RALPH", "TALAN",
         "MASEN", "KADIN", "ROWEN", "SLADE", "MIKEL", "JOSEF", "ETHEN",
         "TYLOR", "DYLON", "BRYAN", "KEVEN", "KEVON", "ISAAK", "DEVON",
         "GAVEN", "CHUCK", "DIEGO", "HADEN", "RILEY", "KOREY", "RAHUL",
         "KASEY", "DARIN", "DARYL", "KEYON", "JAVEN", "JAHIR", "EFREN",
         "JAREN", "ADOLF", "GLENN", "GRADY", "IRVIN", "ELMER", "ELVIS",
         "ANTON", "JOVAN", "TARIQ", "JORDY", "KEANU", "SAMMY", "PERRY",
         "ELVIN", "JAMEL", "BARRY", "CARLO", "STONE", "JAMIL", "FIDEL",
         "BENNY", "TYRON", "KYREE", "CADIN", "TRACY", "ERNIE", "DUANE",
         "KERRY", "GREGG", "LLOYD", "MARTY", "FLOYD", "ROBIN", "CLYDE",
         "STACY", "CECIL", "GARRY", "LOREN", "MYRON", "TEDDY", "MONTE",
         "ROCKY", "CLINT", "ALTON", "RUSTY", "MONTY", "DAREN", "VINCE",
         "ROBBY", "CAREY", "GERRY", "ERVIN", "DENIS", "BUDDY", "DEWEY",
         "RUFUS", "LOUIE", "HOMER", "DENNY", "DONNY", "BLAIR", "KIRBY",
         "PERCY", "MITCH", "SANDY", "CLIFF", "GALEN", "ERICH", "GARTH",
         "KRAIG", "LONNY", "NICKY", "ELTON", "LENNY", "MERLE", "DWAIN",
         "ERWIN", "TONEY", "DARON", "LORNE", "KELLY", "ELDON", "ERROL",
         "LANNY", "DERON", "TIMMY", "WILEY", "EMORY", "HIRAM", "BRITT",
         "ODELL", "WALLY", "BRANT", "BARON", "ELROY", "JAMEY", "BASIL",
         "BRIEN", "OLLIE", "BERRY", "ANDRA", "LEIGH", "DUSTY", "FRITZ",
         "ROYAL", "KAREN", "SUSAN", "PARIS", "DWANE", "FARON", "BUTCH",
         "BROOK", "LINDA", "BORIS", "LORIN", "TOBIN", "BLANE", "BRAIN",
         "KIETH", "DARCY", "AUDIE", "MAURY", "TAMMY", "REGAN", "ARLEN",
         "WOODY", "DERIC", "CYRIL", "GLYNN", "DARRY", "IRWIN", "DONNA",
         "RUBIN", "CLAIR", "ARTIE", "WILLY", "GILES", "JULES", "ARNIE",
         "HOWIE"
    ]

-- jakob/amare/kadin/orion/benny
-- ameer/merle/ernie/elias/reese
