import Data.Char(isAlpha,toLower)
import Data.List(sort)
import Data.Map(Map,fromList)
import qualified Data.Map

prez = ["Washington", "Adams", "Jefferson", "Madison", "Monroe", "Adams",
        "Jackson", "Van Buren", "Harrison", "Tyler", "Polk", "Taylor",
        "Fillmore", "Pierce", "Buchanan", "Lincoln", "Johnson", "Grant",
        "Hayes", "Garfield", "Arthur", "Cleveland", "Harrison", "Cleveland",
        "McKinley", "Roosevelt", "Taft", "Wilson", "Harding", "Coolidge",
        "Hoover", "Roosevelt", "Truman", "Eisenhower", "Kennedy", "Johnson",
        "Nixon", "Ford", "Carter", "Reagan", "Bush", "Clinton", "Bush",
        "Obama"]

caps = ["Abu Dhabi", "Abuja", "Accra", "Adamstown", "Addis Ababa", "Algiers",
        "Alofi", "Amman", "Amsterdam", "Andorra la Vella", "Ankara",
        "Antananarivo", "Apia", "Ashgabat", "Asmara", "Astana", "Asunción",
        "Athens", "Avarua", "Baghdad", "Baku", "Bamako", "Bandar Seri Begawan",
        "Bangkok", "Bangui", "Banjul", "Basseterre", "Beijing", "Beirut",
        "Belfast", "Belgrade", "Belmopan", "Berlin", "Bern", "Bishkek", "Bissau",
        "Bogotá", "Brasília", "Bratislava", "Brazzaville", "Bridgetown",
        "Brussels", "Bucharest", "Budapest", "Buenos Aires", "Bujumbura",
        "Cairo", "Canberra", "Caracas", "Cardiff", "Castries", "Cayenne",
        "Charlotte Amalie", "Chisinau", "Cockburn Town", "Conakry", "Copenhagen",
        "Dakar", "Damascus", "Dhaka", "Dili", "Djibouti", "Dodoma", "Doha",
        "Douglas", "Dublin", "Dushanbe", "Edinburgh",
        "Edinburgh of the Seven Seas", "Episkopi Cantonment", "Flying Fish Cove",
        "Freetown", "Funafuti", "Gaborone", "George Town", "Georgetown",
        "Georgetown", "Gibraltar", "Grytviken", "Guatemala City", "Gustavia",
        "Hagåtña", "Hamilton", "Hanga Roa", "Hanoi", "Harare", "Hargeisa",
        "Havana", "Helsinki", "Honiara", "Islamabad", "Jakarta", "Jamestown",
        "Jerusalem", "Jerusalem", "Kabul", "Kampala", "Kathmandu", "Khartoum",
        "Kiev", "Kigali", "Kingston", "Kingston", "Kingstown", "Kinshasa",
        "Kuala Lumpur", "Kuala Lumpur", "Kuwait City", "La Paz", "Laâyoune",
        "Libreville", "Lilongwe", "Lima", "Lisbon", "Ljubljana", "Lomé",
        "London", "Luanda", "Lusaka", "Luxembourg", "Madrid", "Majuro", "Malabo",
        "Malé", "Mamoudzou", "Managua", "Manama", "Manila", "Maputo", "Marigot",
        "Maseru", "Mata-Utu", "Mbabane", "Melekeok", "Mexico City", "Minsk",
        "Mogadishu", "Monaco", "Monrovia", "Montevideo", "Moroni", "Moscow",
        "Muscat", "N'Djamena", "Nairobi", "Nassau", "Naypyidaw", "New Delhi",
        "Niamey", "Nicosia", "Nicosia", "Nouakchott", "Nouméa", "Nukuʻalofa",
        "Nuuk", "Oranjestad", "Oslo", "Ottawa", "Ouagadougou", "Pago Pago",
        "Palikir", "Panama City", "Papeete", "Paramaribo", "Paris",
        "Philipsburg", "Phnom Penh", "Plymouth", "Podgorica", "Port Louis",
        "Port Moresby", "Port of Spain", "Port Vila", "Port-au-Prince",
        "Porto-Novo", "Prague", "Praia", "Pretoria", "Pristina", "Pyongyang",
        "Quito", "Rabat", "Reykjavík", "Riga", "Riyadh", "Road Town", "Rome",
        "Roseau", "Saipan", "San José", "San Juan", "San Marino", "San Salvador",
        "Sanaá", "Santiago", "Santo Domingo", "Sarajevo", "Seoul", "Singapore",
        "Skopje", "Sofia", "South Tarawa", "Sri Jayawardenepura", "St. George's",
        "St. Helier", "St. John's", "St. Peter Port", "St. Pierre", "Stanley",
        "Stepanakert", "Stockholm", "Sucre", "Sukhumi", "Suva", "São Tomé",
        "Taipei", "Tallinn", "Tashkent", "Tbilisi", "Tegucigalpa", "Tehran",
        "The Valley", "Thimphu", "Tirana", "Tiraspol", "Tokyo", "Tripoli",
        "Tskhinvali", "Tunis", "Tórshavn", "Ulaanbaatar", "Vaduz", "Valletta",
        "Vatican City", "Victoria", "Vienna", "Vientiane", "Vilnius", "Warsaw",
        "Washington, D.C.", "Wellington", "West Island", "Willemstad",
        "Windhoek", "Yamoussoukro", "Yaoundé", "Yaren", "Yerevan", "Zagreb"]

table = fromList [(sort $ filter isAlpha $ map toLower
                        $ p1 ++ p2, p1 ++ ',' : p2) |
                  p1 <- prez, p2 <- prez, p1 > p2,
                  'r' `elem` (map toLower $ p1 ++ p2)]

test s = fmap ((,) s) $ Data.Map.lookup (sort $ 'r' : sort (filter isAlpha $ map toLower s)) table

main = mapM_ (maybe (return ()) print . test) caps
