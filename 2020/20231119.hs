import Data.Char(isAlpha)
import Data.List(sort)

match :: String -> String -> [(String,String,String)]
match a b
  | a == b = []      
  | otherwise = m (sort (drop 1 (reverse (filter isAlpha a)))) (sort (filter isAlpha b)) ""
  where
    m [] bbs ccs = [(a,b,bbs++ccs)]
    m _ [] _ = []
    m (aa:aas) (bb:bbs) ccs
      | aa == bb = m aas bbs ccs
      | otherwise = m (aa:aas) bbs (bb:ccs)

instruments :: [String]
instruments = [{-"afoxé",-}{-"agogô",-}{-"agung",-}
    {-"angklung",-}{-"babendil",-}{-"bak",-}{-"balafon",-}{-"batá",-}
    {-"cabasa",-}{-"cajón",-}
    "carillon","castanets",
    {-"caxirola",-}{-"caxixi",-}{-"chácaras",-}
    "clapstick","clave","claves","cowbell",{-"crotales",-}
    "cymbal",{-"ferrinho",-}{-"flexatone",-}
    {-"octa-vibraphone",-}{-"gandingan",-}{-"ghatam",-}
    "glockenspiel","gong",{-"güiro",-}"handbells",{-"handpan",-}{-"hang",-}
    "kalimba",
    {-"kayamb",-}{-"kayamba",-}{-"kemanak",-}{-"khartal",-}{-"kouxian",-}
    {-"kulintang",-}"maraca",
    "marimba",{-"mbira",-}{-"pate",-}{-"qairaq",-}{-"kairak",-}{-"shekere",-}
    {-"slit drum",-}"spoon",
    "steelpan","tambourine",{-"teponaztli",-}"triangle",
    {-"trash tube",-}{-"txalaparta",-}
    "vibraphone",{-"vibraslap",-}"washboard","wood block",{-"wooden fish",-}
    "xylophone",{-"zill",-}{-"sandpaper",-}"idiophones",
    {-"agida",-}{-"alfaia",-}{-"apinti",-}
    {-"arobapá",-}{-"ashiko",-}{-"atabaque",-}{-"baboula",-}{-"balaban",-}
    {-"balsié",-}{-"bamboula",-}
    {-"bara",-}{-"barrel drum",-}{-"barriles",-}{-"buleador",-}
    "bass drum",{-"bedug",-}{-"bodhrán",-}
    "bongo drums","bongos",{-"boobam",-}{-"chico repique piano",-}
    {-"chenda",-}{-"chande",-}{-"uruttu chenda",-}{-"veekku chenda",-}
    {-"acchan chenda",-}"conga",{-"tumbadora",-}
    {-"cuíca",-}{-"culo'e puya",-}{-"cultrun",-}
    "drum",{-"dabakan",-}{-"daf",-}{-"dap",-}{-"def",-}
    {-"damaru",-}{-"davul",-}{-"dhol",-}{-"tapan",-}{-"atabal",-}{-"tabl",-}
    {-"dayereh",-}{-"den-den daiko",-}
    {-"dhak",-}{-"dhimay",-}{-"dhimaya",-}{-"dhime",-}{-"dhol",-}{-"dholak",-}
    {-"dholaki",-}{-"dimdi",-}
    "djembe",
    {-"dollu",-}"drum kit","dunun","dundun","doundoun",{-"gran cassa",-}
    {-"goblet drum",-}"gong","bass drum",{-"hira-daiko",-}{-"idakka",-}
    {-"ilimba drum",-}
    {-"ingoma",-}{-"shakwe",-}{-"inyahura",-}{-"igihumurizo",-}{-"janggu",-}
    {-"janggo",-}{-"changgo",-}
    {-"junjung",-}{-"kakko",-}{-"kanjira",-}{-"kebero",-}{-"kendang",-}
    {-"gendang",-}{-"khol",-}{-"mrdanga",-}
    {-"krakebs",-}{-"lambeg drum",-}{-"madhalam",-}{-"madal",-}{-"maddale",-}
    {-"maktoum",-}{-"maktoom",-}
    {-"katem",-}{-"maram",-}{-"mirwas",-}{-"mridangam",-}{-"nagara",-}
    {-"naqareh",-}{-"o-daiko",-}
    {-"okedo-daiko",-}{-"octaban",-}{-"padayani thappu",-}{-"pakhavaj",-}
    {-"pandeiro",-}{-"pandero",-}
    {-"parai",-}{-"qilaut",-}{-"rebana",-}{-"sabar",-}{-"sambal",-}
    {-"samphor",-}{-"shime-daiko",-}
    "snare drum",{-"surdo",-}"tabla","taiko",
    {-"talking drum",-}{-"tsukeshime-daiko",-}
    {-"tambor huacana",-}{-"tambori",-}{-"tamborim",-}
    {-"tamborita calentana",-}
    {-"tambou bas a dé fas",-}{-"tambou bas a yon fas",-}{-"tan-tan",-}
    {-"taphon",-}{-"tar",-}
    {-"tbilat",-}{-"thavil",-}{-"timbales",-}{-"pailas",-}
    "timpani","kettledrum",
    {-"tom-tom drum",-}"tom-tom",{-"tombak",-}{-"tsuzumi",-}{-"tsuri-daiko",-}
    {-"unpitched repique",-}{-"uchiwa-daiko",-}"celesta",{-"crystallophone",-}
    {-"glasschord",-}"glass harmonica",{-"hydraulophone",-}{-"plasmaphone",-}
    {-"pyrophone",-}
    {-"quintephone",-}{-"asadullah",-}{-"meerut",-}{-"shishi odoshi",-}
    {-"suikinkutsu",-}
    {-"wobble board",-}{-"aztec death whistle",-}{-"aerophones",-}
    {-"accordina",-}"accordion",
    {-"button accordion",-}{-"cajun accordion",-}
    {-"chromatic button accordion",-}
    {-"diatonic button accordion",-}{-"free bass accordion",-}
    {-"piano accordion",-}{-"schrammel accordion",-}{-"steirische harmonika",-}
    {-"accordola",-}{-"air horn",-}
    {-"alboka",-}{-"algaita",-}{-"almpfeiferl",-}
    "alphorn","alto horn",{-"arghul",-}
    {-"atenteben",-}{-"aulochrome",-}{-"aulos",-}
    "bagpipe",{-"balaban",-}{-"bandoneón",-}{-"bansuri",-}
    "baritone horn",{-"baritone voice",-}"bassoon",{-"tenoroon",-}
    {-"semicontrabassoon",-}
    "contrabassoon","double bassoon",{-"bawu",-}{-"bayan",-}"bazooka",
    {-"beatboxing",-}{-"bifora",-}{-"birbynė",-}{-"blul",-}{-"blown bottle",-}
    {-"bombard",-}{-"chromatic bombard",-}
    {-"buccina",-}"bugle",{-"bukkehorn",-}{-"bullroarer",-}
    "calliope",{-"castrato",-}
    {-"chalumeau",-}{-"cimbasso",-}{-"f cimbasso",-}{-"e♭ cimbasso",-}
    {-"c cimbasso",-}{-"b♭ cimbasso",-}"clarinet",{-"clarinets",-}
    {-"piccolo clarinet",-}{-"sopranino clarinet",-}{-"soprano clarinet",-}
    {-"basset clarinet",-}{-"clarinette d'amour",-}
    "basset horn",
    {-"alto clarinet",-}{-"bass clarinet",-}{-"contra-alto clarinet",-}
    {-"contrabass clarinet",-}{-"octocontra-alto clarinet",-}
    {-"octocontrabass clarinet",-}{-"clarytone",-}
    "concertina",{-"chemnitzer",-}"conch",{-"cornamuse",-}
    "cornet",{-"e♭ soprano cornet",-}{-"a soprano cornet",-}
    {-"c soprano cornet",-}{-"cornett",-}
    {-"cornu",-}{-"corrugaphone",-}{-"countertenor",-}{-"cromorne",-}
    {-"crumhorn",-}{-"soprano",-}{-"alto",-}{-"tenor",-}{-"bass",-}
    {-"greatbass",-}{-"danso",-}{-"death growl",-}"didgeridoo",
    {-"diple",-}{-"dvojnice",-}{-"dizi",-}{-"double bell euphonium",-}
    {-"doulophone",-}{-"cuprophone",-}{-"duduk",-}{-"dulcian",-}
    {-"soprano",-}{-"alto",-}{-"tenor",-}{-"bass",-}{-"dulzaina",-}
    {-"dung-dkar",-}{-"dzhamara",-}"english horn","euphonium","fife",
    {-"firebird",-}
    "trumpet",{-"fiscorn",-}{-"flabiol",-}{-"flageolet",-}{-"flatt trumpet",-}
    "flugelhorn",
    {-"flumpet",-}{-"flutina",-}"flute",{-"alto flute",-}{-"bass flute",-}
    {-"contra alto flute",-}{-"contrabass flute",-}
    {-"sub contra alto flute",-}{-"double contrabass flute",-}
    {-"hyperbass flute",-}{-"folgerphone",-}"french horn",
    {-"fujara",-}{-"gaida",-}
    {-"gaita gastoreña",-}{-"garmon",-}{-"gemshorn",-}{-"gralla",-}
    {-"guanzi",-}{-"houguan",-}
    {-"hano",-}{- "harmoneon",-}"harmonica",
    {-"chromatic harmonica",-}{-"diatonic harmonica",-}
    {-"tremolo harmonica",-}{-"orchestral harmonica",-}
    {-"chengong harmonica",-}
    "harmonium",{-"heckelphone",-}{-"piccolo heckelphone",-}
    {-"terz heckelphone",-}
    {-"helicon",-}{-"horagai",-}{-"hornucopian dronepipe",-}
    {-"hosaphone",-}{-"hotchiku",-}
    {-"hulusi",-}{-"hun",-}{-"inci",-}"irish flute","jug",
    {-"kagurabue",-}{-"kalaleng",-}{-"kaval",-}
    "kazoo",{-"kèn bầu",-}"keyed bugle",{-"khene",-}{-"khloy",-}{-"khlui",-}
    {-"komabue",-}{-"kombu",-}
    {-"koncovka",-}{-"kortholt",-}{-"koudi",-}{-"kuhlohorn",-}{-"kuzhal",-}
    {-"launeddas",-}{-"livenka",-}
    {-"lur",-}
    {-"lusheng",-}{-"lituus",-}{-"martinshorn",-}
    "mellophone","melodica","melodeon",
    {-"mezzo-soprano",-}{-"mijwiz",-}{-"mizmar",-}{-"mizwad",-}
    {-"musette de cour",-}{-"nadaswaram",-}
    {-"nagak",-}{-"natural trumpet",-}{-"ney",-}{-"nguru",-}{-"nohkan",-}
    "nose flute",{-"nplooj",-}
    {-"nulophone",-}"oboe",
    {-"oboes",-}{-"bass/baritone oboe",-}{-"contrabass oboe",-}
    "cor anglais","english horn",{-"oboe d'amore",-}{-"oboe da caccia",-}
    {-"piccolo oboe",-}{-"oboe musette",-}"ocarina",{-"transverse ocarina",-}
    {-"pendant ocarina",-}{-"inline ocarina",-}{-"multi chambered ocarina",-}
    {-"keyed ocarina",-}{-"slide ocarina",-}{-"octavin",-}{-"ophicleide",-}
    {-"orthotonophonium",-}{-"paixiao",-}{-"palendag",-}
    "pan flute",{-"pasiyak",-}{-"water whistle",-}
    {-"pavari",-}{-"pibgorn",-}{-"picco pipe",-}"piccolo",
    {-"piccolo trumpet",-}"pipe organ","organ",
    {-"flue pipes",-}"pitch pipe",{-"pocket cornet",-}
    {-"pocket trumpet",-}{-"post horn",-}
    {-"pu",-}{-"pulalu",-}{-"qeej",-}{-"quena",-}{-"quinticlave",-}{-"raj",-}
    {-"rackett",-}{-"ralé-poussé",-}
    {-"rauschpfeife",-}
    "recorder",{-"garklein",-}{-"sopranino",-}{-"descant",-}{-"venova",-}
    {-"reed contrabass",-}{-"reed organ",-}{-"rhaita",-}{-"robero",-}
    {-"roman tuba",-}{-"rothphones",-}
    {-"soprano rothphone",-}{-"alto rothphone",-}{-"tenor rothphone",-}
    {-"baritone rothphone",-}{-"bass rothphone",-}"ryuteki","sac de gemecs",
    {-"sackbut",-}
    {-"alto sackbut",-}{-"tenor sackbut",-}{-"bass sackbut",-}
    {-"contrabass sackbut",-}
    {-"saenghwang",-}{-"samponia",-}{-"saratovskaya garmonika",-}
    {-"sarrusophones",-}
    {-"sopranino sarrusophone",-}{-"soprano sarrusophone",-}
    {-"alto sarrusophone",-}
    {-"tenor sarrusophone",-}{-"baritone sarrusophone",-}
    {-"bass sarrusophone",-}
    {-"contrabass sarrusophone",-}"saxophone",{-"saxophones",-}
    {-"piccolo saxophone",-}
    {-"soprillo",-}{-"sopranino saxophone",-}{-"c soprano saxophone",-}
    {-"soprano saxophone",-}{-"mezzo-soprano saxophone",-}
    {-"alto saxophone",-}{-"c melody saxophone",-}{-"tenor saxophone",-}
    {-"baritone saxophone",-}{-"bass saxophone",-}{-"contrabass saxophone",-}
    {-"subcontrabass saxophone",-}"saxhorn",{-"saxotromba",-}
    {-"saxtuba",-}{-"scat singing",-}{-"schwyzerörgeli",-}
    {-"serpent",-}"shakuhachi",{-"shankha",-}
    {-"shawm",-}{-"shehnai",-}{-"sheng",-}{-"shinobue",-}"shofar",
    {-"shō",-}{-"shvi",-}{-"siku",-}{-"siren",-}
    {-"slide trumpet",-}{-"medieval slide trumpet",-}
    {-"renaissance slide trumpet",-}{-"baroque slide trumpet",-}
    "slide whistle",{-"jazz flute",-}{-"swanee whistle",-}
    {-"sneng",-}{-"sodina",-}{-"sopila",-}"soprano",{-"sorna",-}
    "sousaphone",{-"sralai",-}
    {-"sudrophone",-}{-"suling",-}{-"suona",-}{-"laba",-}{-"haidi",-}
    {-"superbone",-}{-"swordblade",-}{-"tabor pipe",-}
    {-"taepyeongso",-}{-"tárogató",-}{-"tenor",-}
    {-"tenora",-}{-"throat singing",-}
    {-"tible",-}"tin whistle",{-"toasting",-}{-"tonette",-}{-"trikiti",-}
    "trombone",{-"trombones",-}{-"piccolo trombone",-}{-"soprano trombone",-}
    {-"alto trombone",-}{-"tenor trombone",-}{-"bass trombone",-}
    {-"contrabass trombone",-}{-"valve trombone",-}
    {-"superbone",-}{-"tromboon",-}{-"trompeta china",-}"trumpet",
    {-"trumpets",-}{-"soprano trumpet",-}{-"bass trumpet",-}
    {-"baroque trumpet",-} {-"bass trumpet",-}{-"rotary valve trumpet",-}
    "tuba",
    {-"bass tuba",-}{-"contrabass tuba",-}{-"subcontrabass tuba",-}
    {-"tubax",-}{-"contrabass",-}{-"subcontrabass",-}{-"tube trumpet",-}
    {-"tumpong",-}{-"tungso",-}{-"tutek",-}{-"txistu",-}
    "uilleann pipes","venova",{-"venu",-}
    {-"vibrandoneon",-}{-"vienna horn",-}{-"vocal percussion",-}
    "vuvuzela",{-"wagner tuba",-}
    {-"washint",-}{-"western concert flutes",-}
    "flute",
    {-"alto flute",-}{-"bass flute",-}{-"contra-alto flute",-}
    {-"contrabass flute",-}{-"subcontrabass flute",-}
    {-"double contrabass flute",-}{-"hyperbass flute",-}
    {-"whip",-}"whistle",{-"pea whistle",-}
    {-"steam whistle",-}{-"train whistle",-}"willow flute",
    {-"xaphoon",-}{-"xeremia",-}{-"xiao",-}
    {-"xun",-}{-"yotar",-}{-"yu",-}{-"zhaleika",-}{-"zufolo",-}
    {-"zugtrompete",-}{-"zurna",-}{-"adungu",-}
    "aeolian harp",
    {-"ajaeng",-}{-"akkordolia",-}{-"algerian mandole",-}{-"angélique",-}
    {-"appalachian dulcimer",-}{-"arbajo",-}{-"archlute",-}
    {-"arpeggione",-}"autoharp",
    {-"bağlama",-}{-"bajo sexto",-}"balalaika",
    {-,"bandola",-}{-"bandolin",-}{-"bandolón",-}
    {-"bandura",-}{-"bandora",-}{-"bandurria",-}{-"banhu",-}"banjo",
    {-"banjo cello",-}{-"bass banjo",-}{-"five-stringed banjo",-}
    {-"bluegrass banjo",-}{-"four-stringed banjo",-}
    {-"plectrum banjo",-}{-"six-stringed banjo",-}{-"tenor banjo",-}
    {-"zither banjo",-}{-"banjo ukulele",-}
    {-"barbat",-}{-"baryton",-}{-"berimbau",-}{-"bipa",-}"biwa",{-"bordonua",-}
    "bouzouki",{-"buzuq",-}{-"carimba",-}{-"cavaquinho",-}
    "cello","violoncello",
    {-"cello da spalla",-}{-"electric cello",-}
    {-"chapman stick",-}{-"charangos",-}"charango",
    {-"charangón",-}{-"hualaycho",-}{-"walaycho",-}{-"ronroco",-}
    {-"hatun charango",-}"chillador",
    "ayacucho",{-"bajo charango",-}"chango",{-"charango mediano",-}
    {-"khonkhota",-}
    {-"moquegua",-}{-"pampeno",-}{-"shreiker",-}{-"sonko",-}
    {-"vallegrandino",-}
    {-"chitarra battente",-}{-"chitarra italiana",-}{-"choghur",-}
    {-"cimbalom",-}
    {-"electric cymbalum",-}{-"cimboa",-}{-"citole",-}{-"cittern",-}
    "clavichord","clavinet",
    "concheras",{-"mandolinos de concheros",-}{-"mandolina conchera",-}
    {-"vihuelas de concheros",-}{-"vihuela conchera",-}
    {-"guitarras de concheros",-}
    {-"guitarra conchera",-}
    {-"contraguitar",-}"crwth","crowd","cuatro",{-"cümbüş",-}
    {-"đàn bầu",-}{-"đàn đáy",-}{-"đàn gáo",-}{-"đàn nguyệt",-}
    {-"đàn tam thập lục",-}{-"đàn tranh",-}{-"đàn tre",-}{-"đàn tỳ bà",-}
    {-"diddley bow",-}{-"dihu",-}{-"dombra",-}{-"domra",-}{-"doshpuluur",-}
    {-"dotara",-}"double bass",{-"five-string double bass",-}
    "dreadnought","dulcimer",
    {-"dutar",-}{-"duxianqin",-}{-"ektara",-}{-"erhu",-}{-"erxian",-}
    {-"esraj",-}{-"faglong",-}{-"fuglung",-}
    {-"fegereng",-}"fiddle",
    {-"gaohu",-}{-"gayageum",-}{-"geomungo",-}{-"gittern",-}{-"gottuvadhyam",-}
    "guitar",{-"guitars",-}{-"acoustic guitar",-}{-"acoustic bass guitar",-}
    {-"acoustic-electric guitar",-}{-"archtop guitar",-}{-"baritone guitar",-}
    {-"baroque guitar",-}{-"bass guitar",-}{-"bahian guitar",-}
    {-"brahms guitar",-}{-"chitarra battente",-}{-"cigar box guitar",-}
    {-"classical guitar",-}{-"console steel guitar",-}{-"electric guitar",-}
    {-"english guitar",-}{-"fretless guitar",-}{-"lyre-guitar",-}
    {-"extended-range guitars",-}{-"alto guitar",-}{-"seven-string guitar",-}
    {-"eight-string guitar",-}{-"nine-string guitar",-}
    {-"ten-string guitar",-}{-"eleven-string alto guitar",-}
    {-"twelve-string guitar",-}{-"flamenco guitar",-}
    {-"guitarra quinta huapanguera",-}{-"guitar synthesizer",-}
    "guitarrón",{-"gut-stringed guitars",-}
    {-"lap steel guitars",-}"dobro",{-"national steel",-}
    {-"multi-neck guitar",-}
    {-"double-neck guitar",-}{-"triple-neck guitar",-}
    {-"quadruple-neck guitar",-}
    {-"five-neck guitar",-}{-"six-neck guitar",-}{-"seven-neck guitar",-}
    {-"eight-neck guitar",-}"rock ock",{-"twelve-neck guitar",-}
    {-"octave guitar",-}{-"parlor guitar",-}{-"pedal steel guitar",-}
    {-"resophonic guitar",-}{-"romantic guitar",-}
    {-"russian guitar",-}{-"selmer guitar",-}{-"semi-acoustic guitar",-}
    {-"slide guitar",-}{-"silent guitar",-}{-"steel guitar",-}
    {-"steel-string acoustic guitar",-}{-"tenor guitar",-}{-"terz guitar",-}
    "yotar",{-"guitarra de golpe",-}
    {-"guitarra panzona",-}{-"guitarra séptima",-}"guitarro",
    "gusli","guqin","guzheng",
    "haegeum","hammered dulcimer","hardanger fiddle","harmonico","harp",
    {-"electric harp",-}{-"harp guitar",-}"harpsichord","hegelong",
    "huapanguera",
    {-"huluhu",-}{-"huqin",-}"hurdy-gurdy",
    {-"icelandic fiddle",-}{-"fiðla",-}{-"igil",-}
    {-"irish bouzouki",-}{-"janzi",-}{-"jarana jarocho",-}
    {-"jarana huasteca",-}
    {-"jarana mosquito",-}{-"jarana segunda",-}{-"jarana tercera",-}
    {-"jiaohu",-}{-"kabosy",-}
    {-"kadlong",-}{-"kamancha",-}{-"kantele",-}{-"kemenche",-}{-"khim",-}
    {-"kobza",-}{-"kokle",-}{-"kokyū",-}
    {-"komuz",-}{-"kora",-}
    "koto",{-"kubing",-}{-"kudyapi",-}{-"kwitra",-}{-"langeleik",-}
    {-"laouto",-}
    {-"laruan",-}{-"laúd",-}{-"lavta",-}{-"leiqin",-}{-"leona",-}{-"lirone",-}
    {-"liuqin",-}{-"lokanga",-}
    "lute",{-"lute guitar",-}{-"lyra",-}"lyre",{-"maguhu",-}{-"mandobass",-}
    "mandola",
    "mandolin",{-"mandolin-banjo",-}{-"mandocello",-}{-"mandola",-}
    {-"bluegrass mandolin",-}
    {-"electric mandolin",-}{-"octave mandolin",-}{-"resonator mandolin",-}
    {-"mandolute",-}{-"mandora",-}{-"mandore",-}{-"marovany",-}
    {-"mejoranera",-}
    {-"mexican vihuela",-}{-"mohan veena",-}{-"moraharpa",-}{-"morin khuur",-}
    {-"musical bow",-}{-"nyckelharpa",-}
    {-"octobass",-}"oud",{-"paqin",-}"piano","pianoforte",{-"electric piano",-}
    {-"fortepiano",-}{-"pedal piano",-}{-"pipa",-}
    {-"piwancha",-}{-"pochette",-}
    {-"portuguese guitar",-}{-"psaltery",-}{-"qanun",-}{-"qinqin",-}
    {-"rabeca",-}{-"rajão",-}
    {-"ravanahatha",-}{-"rebab",-}{-"rebec",-}{-"requinto jarocho",-}
    {-"rubab",-}{-"ruan",-}
    {-"gaoyinruan",-}{-"xiaoruan",-}{-"zhongruan",-}{-"daruan",-}
    {-"diyinruan",-}{-"rudra vina",-}
    {-"sallameh",-}{-"sanshin",-}{-"santoor",-}{-"sanxian",-}{-"sarangi",-}
    {-"šargija",-}{-"sarod",-}
    {-"saung",-}"saw",{-"saw sam sai",-}{-"se",-}{-"seul",-}{-"setar",-}
    "lute","shamisen",{-"sintir",-}
    "sitar",{-"sitarla",-}{-"surbahar",-}{-"swarmandal",-}{-"tamburica",-}
    {-"tambur",-}{-"tanpura",-}
    {-"tar",-}{-"tea chest bass",-}{-"tembûr",-}{-"theorbo",-}{-"timple",-}
    {-"tiple",-}{-"tovshuur",-}
    {-"tres",-}{-"tres cubano",-}{-"tres puerto rico",-}"tricordia",{-"tro",-}
    {-"trumpet marine",-}
    {-"tromba marina",-}{-"tsymbaly",-}{-"tuhu",-}{-"tzouras",-}
    "ukulele",{-"concert ukulele",-}
    {-"electric ukulele",-}{-"harp ukulele",-}{-"lap steel ukulele",-}
    {-"pocket ukulele",-}{-
    "resonator ukulele",-}{-"soprano ukulele",-}{-"tahitian ukulele",-}
    {-"tenor ukulele",-}{-"eight-string tenor",-}{-"five-string tenor",-}
    "lili'u",{-"six-string tenor",-}
    {-"baritone ukulele",-}{-"bass ukulele",-}{-"contrabass ukulele",-}
    {-"u-bass",-}{-"cigar box ukulele",-}{-"ukelin",-}{-"valiha",-}{-"veena",-}
    {-"vertical viola",-}{-"vichitra veena",-}{-"vielle",-}{-"vihuela",-}
    "viol",{-"pardessus de viole",-}
    {-"treble viol",-}{-"dessus",-}{-"alto viol",-}{-"bass viol",-}
    {-"division viol",-}{-"lyra viol",-}
    {-"tenor viol",-}{-"taille",-}{-"great bass violone",-}
    {-"contrabass violone",-}"viola",
    {-"viola da gamba",-}{-"viola amarantina",-}{-"viola bastarda",-}
    {-"viola beiroa",-}
    {-"viola caipira",-}{-"viola d'amore",-}{-"viola da terra",-}
    {-"viola de arame",-}
    {-"viola de cocho",-}{-"viola organista",-}{-"viola profonda",-}
    "violin",
    {-"piccolo violino",-}{-"baroque violin",-}
    {-"bass violin",-}{-"electric violin",-}
    {-"five string violin",-}{-"stroh violin",-}{-"tenor violin",-}
    {-"violone",-}{-"violotta",-}
    {-"walaycho",-}{-"waldzither",-}{-"washtub bass",-}{-"whamola",-}
    "wheelharp",{-"xalam",-}
    {-"khalam",-}{-"yaylı tambur",-}{-"yangqin",-}{-"yazheng",-}{-"yehu",-}
    {-"yelatáj chos woley",-}
    {-"yueqin",-}{-"zhongruan",-}{-"zhonghu",-}{-"zhu",-}{-"zhengni",-}
    {-"zhuihu",-}"zither"
    {-"alpine zither",-}{-"harp zither",-}{-"concert zither",-}
    {-"guitar zither",-}{-"overtone zither",-}{-"violinzither"-}]

-- cello bow cowbell
