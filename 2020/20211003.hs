import Data.Char(isAlpha,toLower)

caps = ["Ankara", "Asmara", "Astana", "Athens", "Avarua", "Bamako", "Bangui",
        "Banjul", "Beirut", "Berlin", "Bissau", "Bogotá", "Dodoma", "Dublin",
        "Harare", "Havana", "Kigali", "Lisbon", "London", "Luanda", "Lusaka",
        "Madrid", "Majuro", "Malabo", "Manama", "Manila", "Maputo", "Maseru",
        "Monaco", "Moroni", "Moscow", "Muscat", "Nassau", "Niamey", "Nouméa",
        "Ottawa", "Prague", "Riyadh", "Roseau", "Saipan",
        "San José", "San Juan",
        "Skopje", "Taipei", "Tehran", "Tirana", "Vienna", "Warsaw", "Zagreb"]

countries = ["Ivory Coast", "United Arab Emirates", "Nigeria", "Ghana", 
        "Pitcairn Islands", "Ethiopia", "Yemen", "Algeria", "Niue", "Jordan", 
        "Netherlands", "Turkey", "Madagascar", "Samoa", "Turkmenistan",
        "Eritrea", "Paraguay", "Greece", "Cook Islands", "Iraq",
        "Azerbaijan", "Mali", "Brunei", "Thailand",
        "Central African Republic", "The Gambia", "Saint Kitts and Nevis",
        "China", "Lebanon", "Serbia", "Belize", "Germany", "Switzerland",
        "Kyrgyzstan", "Guinea-Bissau", "South Africa", "Colombia",
        "Montserrat", "Brazil", "Slovakia", "Congo", "Barbados", "Belgium",
        "Romania", "Hungary", "Argentina", "Egypt", "Australia", "Venezuela",
        "Saint Lucia", "Montenegro", "United States Virgin Islands",
        "Moldova", "Turks and Caicos Islands", "Sri Lanka", "Guinea",
        "Denmark", "Senegal", "Syria", "Tanzania", "Bangladesh", "East Timor",
        "Djibouti", "Qatar", "Isle of Man", "Ireland", "Tajikistan",
        "Western Sahara", "Christmas Island", "Sierra Leone", "Tuvalu",
        "Botswana", "Cayman Islands", "Ascension Island", "Guyana",
        "Gibraltar", "Burundi", "Guatemala", "Saint Barthélemy", "Guam",
        "Bermuda", "Vietnam", "Zimbabwe", "Somaliland", "Cuba", "Finland",
        "Solomon Islands", "Pakistan", "Indonesia", "Saint Helena", "Israel",
        "South Sudan", "Afghanistan", "Uganda", "Nepal", "Sudan", "Rwanda",
        "South Georgia and the South Sandwich Islands", "Jamaica",
        "Norfolk Island", "Saint Vincent and the Grenadines",
        "Democratic Republic of the Congo", "Malaysia", "Kuwait", "Ukraine",
        "Bolivia", "Gabon", "Malawi", "Peru", "Portugal", "Slovenia",
        "Eswatini", "Togo", "United Kingdom", "Angola", "Zambia",
        "Luxembourg", "Spain", "Marshall Islands", "Equatorial Guinea",
        "Maldives", "Nicaragua", "Bahrain", "Philippines", "Mozambique",
        "Åland Islands", "Saint Martin", "Lesotho", "Wallis and Futuna",
        "Mexico", "Belarus", "Somalia", "Monaco", "Liberia", "Uruguay",
        "Comoros", "Russia", "Oman", "Kenya", "Bahamas", "Myanmar", "Chad",
        "India", "Palau", "Niger", "Cyprus", "Mauritania", "New Caledonia",
        "Tonga", "Kazakhstan", "Greenland", "Aruba", "Norway", "Canada",
        "Burkina Faso", "American Samoa", "Micronesia", "Panama",
        "French Polynesia", "Suriname", "France", "Sint Maarten", "Cambodia",
        "Mauritius", "Papua New Guinea", "Trinidad and Tobago", "Vanuatu",
        "Haiti", "Benin", "Czech Republic", "Cape Verde", "Kosovo",
        "North Korea", "Ecuador", "Morocco", "Iceland", "Latvia",
        "Saudi Arabia", "British Virgin Islands", "Italy", "Dominica",
        "Northern Mariana Islands", "Costa Rica", "Puerto Rico", "San Marino",
        "El Salvador", "Chile", "São Tomé and Príncipe",
        "Bosnia and Herzegovina", "South Korea", "Singapore",
        "North Macedonia", "Bulgaria", "Kiribati", "Grenada", "Jersey",
        "Antigua and Barbuda", "Guernsey", "Saint Pierre and Miquelon",
        "Falkland Islands", "Artsakh", "Sweden", "Abkhazia", "Fiji", "Taiwan",
        "Estonia", "Uzbekistan", "Georgia", "Honduras", "Iran", "Bhutan",
        "Albania", "Transnistria", "Japan", "Faroe Islands", "Libya",
        "South Ossetia", "Tunisia", "Mongolia", "Liechtenstein", "Malta",
        "Anguilla", "Vatican City", "Seychelles", "Austria", "Laos",
        "Lithuania", "Poland", "United States", "New Zealand",
        "Cocos (Keeling) Islands", "Curaçao", "Namibia", "Cameroon", "Nauru",
        "Armenia", "Croatia"]

possible :: String -> String -> Bool
possible cap country =
    test (canonical (drop 1 (reverse cap))) (canonical (reverse country))
  where
    canonical = map toLower . filter isAlpha
    test rcap rcountry
      | null rcap = False
      | rcap == take (length rcap) rcountry = True
      | otherwise = test (tail rcap) rcountry

main :: IO ()
main = mapM_ print [(country,cap) | country <- countries, cap <- caps, possible cap country]

-- Pakistan Karachi
