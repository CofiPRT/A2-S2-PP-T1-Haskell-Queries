read_table ' ' '\n' "user_id movie_id rating\n196 242 3\n196 302 3\n196 377 1"
read_table ' ' '\n' "user_id movie_id rating\n197 242 4\n197 352 3\n197 377 2"

testTable1 = read_table ' ' '\n' "user_id movie_id rating\n196 242 3\n196 302 3\n196 377 1"
testTable2 = read_table ' ' '\n' "user_id movie_id rating\n197 242 4\n197 352 3\n197 377 2"

testTable3 = read_table ' ' '\n' "user_id movie_id rating\n197 242 4\n197 352 3\n198 352 2"

testMovieIDs = ["242", "302", "352", "377"]

testFunc = cosCmpU testTable1 testTable2 testMovieIDs 0 0 0

testFunc2 = eval $ Cosine $ Filter (Not (Lt "rating" 3)) $ Atom rating

testFunc3 = eval (Cosine (Atom testTable3))


-- filtreaza un tabel dupa un anumit user_id
userTable :: Table -> Field -> Table
userTable table user = eval (Filter (Eq "user_id" user) (Atom table))


-- filtreaza un tabel dupa un anumit movie_id
movieTable :: Table -> Field -> Table
movieTable table movie = eval (Filter (Eq "movie_id" movie) (Atom table))

{-
table: un tabel sursa
users: o lista de utilizatori

Pentru fiecare utilizator, acesta se compara cu toti utilizatorii urmatori,
cu ajutorul functiei cosCmp (in maniera n + (n - 1) + (n - 2) + ... + 1)
-}
cosAux :: Table -> [Field] -> [Entry]
cosAux table [] = []
cosAux table users = (cosCmp table table1 rest) ++ (cosAux table rest)
    where
        table1 = userTable table (head users)
        rest = tail users

{-
table: un tabel sursa
table1: tabelul primului user
users: o lista cu restul user-ilor

Se ia cate un user din users si i se obtine tabelul doar cu intrarile acestuia,
din tabelul original table in table2.
La fiecare pas, se aplica cosCmpU pe aceste 2 tabele.
Astfel, table1 primit ca parametru este comparat cu fiecare tabel pentru fiecare
user ulterior din users.
-}
cosCmp :: Table -> Table -> [Field] -> [Entry]
cosCmp table table1 [] = []
cosCmp table table1 users = currentStep ++ nextStep
    where
        currentStep = cosCmpU table1 table2 movieIDs 0 0 0
        nextStep = cosCmp table table1 rest

        table2 = userTable table (head users)
        rest = tail users

        movieIDs = sort (map head (getEntries movieTable))

        movieTable = eval (movieQuery1 :|| movieQuery2)
        movieQuery1 = Select ["movie_id"] (Atom table1)
        movieQuery2 = Select ["movie_id"] (Atom table2)

{-
table1: tabel in care toate intrarile au acelasi user_id 
table2: similar, dar pentru alt user_id
movieIDs: lista de movie_id prin care se va itera
prod, sum1, sum2: valori folosite in recursivitate pentru rezultatul final

Se ia cate un movie_id si se cauta intrarile aferente acestuia si din table1
si din table2, adaugand rating-urile la rezultatul final
-}
cosCmpU :: Table -> Table -> [Field] -> Float -> Float -> Float -> [Entry]
cosCmpU table1 table2 movieIDs prod sum1 sum2 = decide
    where
        decide = if movieIDs == [] then evaluate else nextStep

        -- calcul final
        evaluate = [[user1, user2, resultString]]

        resultString = showFFloat (Just 4) result ""

        result = prod / ((sqrt sum1) * (sqrt sum2))

        user1 = getElem "user_id" movSchema1' (head movEntries1')
        user2 = getElem "user_id" movSchema2' (head movEntries2')

        (Table movSchema1' movEntries1') = table1
        (Table movSchema2' movEntries2') = table2

        -- iterare recursiva
        nextStep = cosCmpU table1 table2 (tail movieIDs) prod' sum1' sum2'

        prod' = prod + (rating1int * rating2int)
        sum1' = sum1 + (rating1int ** 2)
        sum2' = sum2 + (rating2int ** 2)

        rating1int = if rating1 == [] then 0 else (read rating1)::Float
        rating2int = if rating2 == [] then 0 else (read rating2)::Float

        rating1 = if movEntries1 == []
                    then []
                    else getElem "rating" movSchema1 (head movEntries1)
        rating2 = if movEntries2 == []
                    then []
                    else getElem "rating" movSchema2 (head movEntries2)

        (Table movSchema1 movEntries1) = movieTable table1 (head movieIDs)
        (Table movSchema2 movEntries2) = movieTable table2 (head movieIDs)


instance Show Table where
    show (Table header ents) = lineSep ++ moddedHeader ++
                                lineSep ++ newEnts ++
                                lineSep
        where
            -- linia plina de "-"
            lineSep = (replicate (foldr (\x y -> x + y + 1) 1 sizes) '-')
                        ++ "\n"

            -- lista cu lungimile maxime pt fiecare coloana
            sizes = foldr func acc list

            func = zipWith max
            acc = map length header
            list = map (map length) ents

            -- string cu header-ul ce trebuie afisat (in format aferent)
            moddedHeader = (addColSep (paddedList header sizes)) ++ "\n"

            -- string cu entry-urile ce trebuie afisate (in format aferent)
            newEnts = foldr (++) []
                (map (\x -> (addColSep (paddedList x sizes)) ++ "\n") ents)

            -- primeste un string si adauga spatii la final pana cand string-ul
            -- ajunge la size-ul dorit
            extendToSize :: String -> Int -> String
            extendToSize str size = if size > (length str)
                                    then str ++ (r str size)
                                    else str

            r str size = replicate (size - (length str)) ' '

            -- primeste o lista de string-uri si una de lungimi si aplica
            -- padding-ul aferent fiecaruia
            paddedList :: [String] -> [Int] -> [String]
            paddedList strList sizes = zipWith (extendToSize) strList sizes

            -- primeste o lista de string-uri si formateaza conform cerintei
            addColSep :: [String] -> String
            addColSep strList = foldr (\x y -> "|" ++ x ++ y) "|" strList

-- Construire array de lungimi maxime
max_length_array :: [Entry] -> [Int] -> [Int]
max_length_array [] xs = xs
max_length_array entries xs = max_length_array (tail entries) (f (head entries) xs)
    where
        f [] _ = []
        f (e:l) (x:xs)
            | (length e) > x = (length e) : (f l xs)
            | otherwise = x : (f l xs)

-- Prelucrare tabel
concat_table :: [Entry] -> [Int] -> [Char]
concat_table [] _ = ""
concat_table (e:entries) xs = (f e xs) ++ "\n" ++ (concat_table entries xs)
    where
        f [] _ = "|"
        f (e:l) (x:xs) = "|" ++ e ++ (replicate (x - (length e)) ' ') ++ (f l xs)

-- Implementare Show
instance Show Table where
    show (Table header entries)
    		= y ++ (concat_table [header] x) ++ y ++ (concat_table entries x) ++ y
        where
            x = max_length_array (header:entries) (replicate (length header) 0)
            y = replicate ((foldl (+) 0 x) + ((length header) + 1)) '-' ++ "\n"


testEnts = [["ana", "thriller", "4"], ["ana", "tvd", "10"], ["bob", "tvd", "0"]]
testFunc = sameUserAux testEnts (head (head testEnts))

ents4 = [["ana", "thriller", "4"], ["ana", "tvd", "10"]] ++ ents3
testCosAux = cosAux ents4

ents3 = [["bob", "tvd", "1"], ["bob", "america", "3"], ["clara", "boomer", "4"]]
testCosCmp = cosCmp ents1 ents3

ents1 = [["ana", "thriller", "4"], ["ana", "tvd", "10"], ["ana", "116"]]
ents2 = [["bob", "tvd", "1"], ["bob", "america", "3"], ["bob", "10"]]
testCosCmpU = cosCmpU ents1 ents2 0

testCosineBaseSch = ["user_id", "movie_id", "rating"]
testCosineSortedEnts = sort (getEntries testCosine1)

testCosine1 = eval $ Filter (Not (Lt "rating" 3)) $ Atom rating
testCosine2 = Table testCosineBaseSch (cosAux testCosineSortedEnts)



testCosine3 = length (cosAux testCosineSortedEnts)

		{-ents = if sch == baseSch
                then getEntries table
                else getEntries rearrangedTable

        baseSch = ["user_id", "movie_id", "rating"]
        rearrangedTable = eval (Select baseSch (Atom table))-}


{- primeste o lista de intrari si compara primul utilizator din aceasta cu
urmatorii; la pasul urmator se trece la urmatorul utilizator, care se compara
DOAR cu cei de dupa el etc. -}
cosAux :: [Entry] -> [Entry]
cosAux [] = []
cosAux entries = currentStep ++ nextStep
    where
        currentStep = cosCmp userEntries rest
        nextStep = cosAux rest

        rest = drop ((length userEntries) - 1) entries
        userEntries = sameUser entries

{- primeste intrarile unui utilizator si restul intrarilor si aplica cosCmpU
asupra celor 2 utilizatori: cel primit si primul din restul intrarilor; la
pasul urmator se aplica cosCmpU asupra aceluiasi utilizator dar impreuna cu
URMATORUL din lista de intrari etc. -}
cosCmp :: [Entry] -> [Entry] -> [Entry]
cosCmp uEntries1 [] = []
cosCmp uEntries1 entries = currentStep ++ nextStep
    where
        currentStep = cosCmpU uEntries1 uEntries2 0
        nextStep = cosCmp uEntries1 rest

        rest = drop ((length uEntries2) - 1) entries
        uEntries2 = sameUser entries

{- cauta recursiv toate filmele ale utilizatorului 1 care sunt prezente si la
utilizatorul 2 pentru a le calcula produsul scalar; se foloseste de getRating
si de getUserSum -}
cosCmpU :: [Entry] -> [Entry] -> Float -> [Entry]
cosCmpU uEntries1 uEntries2 pSum = if (length currEntry1) == 2
                                        then evaluate
                                        else nextStep
    where
        evaluate = [[user1, user2, resultString]]

        user1 = currEntry1 !! 0
        user2 = head (head uEntries2)

        resultString = showFFloat (Just 4) result ""
        result = pSum / ((sqrt sum1) * (sqrt sum2))
        sum1 = read (currEntry1 !! 1)::Float
        sum2 = getUserSum uEntries2


        nextStep = cosCmpU (tail uEntries1) uEntries2 pSum'

        pSum' = pSum + (rating1 * rating2)
        rating1 = read (currEntry1 !! 2)::Float
        rating2 = getRating uEntries2 movieID

        movieID = currEntry1 !! 1

        currEntry1 = head uEntries1

{- extrage din intrarea primita rating-ul filmului dorit -}
getRating :: [Entry] -> Field -> Float
getRating uEntries movieID = if (length currEntry) == 2 -- sfarsitul intrarilor
                                then 0
                                else decide
    where
        decide = if (currEntry !! 1) == movieID
                    then read (currEntry !! 2)::Float
                    else getRating (tail uEntries) movieID

        currEntry = head uEntries

{- extrage din intrarile calculate cu sameUser norma utilizatorului -}
getUserSum :: [Entry] -> Float
getUserSum uEntries = if (length currEntry) == 2
                        then sumNum
                        else getUserSum (tail uEntries)
    where
        sumNum = read (currEntry !! 1)::Float
        currEntry = head uEntries



module Query where

import UserInfo
import Rating
import Movie

import Numeric -- pentru 'showFFloat'
import Data.List -- pentru 'transpose', 'union' si 'sort'

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

-- FUNCTII AJUTATOARE GENERICE

-- elimina duplicatele dintr-o lista, transformand-o in set
makeSet :: Eq a => [a] -> [a]
makeSet list = makeSetAux [] list
    where
        makeSetAux acc [] = acc
        makeSetAux acc (x:xs) = if elem x acc
                                then makeSetAux acc xs
                                else makeSetAux (acc ++ [x]) xs

getEntries :: Table -> [Entry]
getEntries (Table schema entries) = entries

getSchema :: Table -> TableSchema
getSchema (Table schema entries) = schema

-- TODO 1

read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table colSep lineSep str = Table (head list) (tail list)
    where
        -- impartire dupa lineSep, si ULTERIOR dupa colSep
        list = map (splitBy colSep) (splitBy lineSep str)

        -- imparte un string intr-o lista de string-uri, dupa separator
        splitBy :: Char -> String -> [String]
        splitBy sep [] = []
        -- y incepe cu separatorul dat, n-avem nevoie de acesta
        splitBy sep str = x : (splitBy sep (drop 1 y)) 
            where
                (x, y) = span (/= sep) str {- x = partea de string de dinaintea
                                        separatorului, y = restul string-ului -}

user_info = read_table '|' '\n' user_info_str
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str

-- TODO 2

instance Show Table where
    show (Table header ents) = lineSep ++ moddedHeader ++
                                lineSep ++ newEnts ++
                                lineSep
        where
            -- linia plina de "-"
            lineSep = (replicate (foldr (\x y -> x + y + 1) 1 sizes) '-')
                        ++ "\n"

            -- lista cu lungimile maxime pt fiecare coloana
            sizes = foldr func acc list

            func = zipWith max
            acc = map length header
            list = map (map length) ents

            -- string cu header-ul ce trebuie afisat (in format aferent)
            moddedHeader = (addColSep (paddedList header sizes)) ++ "\n"

            -- string cu entry-urile ce trebuie afisate (in format aferent)
            newEnts = foldr (++) []
                (map (\x -> (addColSep (paddedList x sizes)) ++ "\n") ents)

            -- primeste un string si adauga spatii la final pana cand string-ul
            -- ajunge la size-ul dorit
            extendToSize :: String -> Int -> String
            extendToSize str size = if size > (length str)
                                    then str ++ (r str size)
                                    else str

            r str size = replicate (size - (length str)) ' '

            -- primeste o lista de string-uri si una de lungimi si aplica
            -- padding-ul aferent fiecaruia
            paddedList :: [String] -> [Int] -> [String]
            paddedList strList sizes = zipWith (extendToSize) strList sizes

            -- primeste o lista de string-uri si formateaza conform cerintei
            addColSep :: [String] -> String
            addColSep strList = foldr (\x y -> "|" ++ x ++ y) "|" strList

-- TODO 3

data FilterCondition = Lt Field Integer |
                        Eq Field String |
                        In Field [String] |
                        Not FilterCondition

getElem :: Field -> TableSchema -> Entry -> Field
getElem field [] _ = [] -- nu ar trebui sa ajunga aici
getElem field _ [] = []
getElem field sch entry = if (head sch) == field
                                then (head entry)
                                else getElem field (tail sch) (tail entry)


getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Lt field int) sch = functionLt
    where
        -- Lower Than
        functionLt :: Entry -> Bool
        functionLt entry = if (read (getElem field sch entry)::Integer) < int
                            then True
                            else False

getFilter (Eq field string) sch = functionEq
    where
        -- Equal
        functionEq :: Entry -> Bool
        functionEq entry = if (getElem field sch entry) == string
                            then True
                            else False
        
getFilter (In field strList) sch = functionIn
    where
        -- In Values
        functionIn :: Entry -> Bool
        functionIn entry = elem (getElem field sch entry) strList

getFilter (Not condition) sch = functionNot
    where
        -- Not
        functionNot :: Entry -> Bool
        functionNot entry = not ((getFilter condition sch) entry)

-- TODO 4

data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom table) = table

eval (Filter cond (Atom table)) = Table sch newEnts
    where
        sch = getSchema table
        ents = getEntries table

        newEnts = filter (getFilter cond sch) ents

-- apel recursiv
eval (Filter cond q) = eval (Filter cond (Atom (eval q)))



eval (Select cols (Atom table)) = Table cols (transpose newEnts)
    where
        sch = getSchema table
        ents = getEntries table

        newEnts = map (searchColumn sch ents) cols

        searchColumn :: TableSchema -> [Entry] -> String -> [String]
        searchColumn _ [] col = []
        searchColumn [] _ col = []
        searchColumn sch' ents' col = if (head sch') == col
                                        then found ents'
                                        else recursive sch' ents' col

        found ents' = map head ents'
        recursive sch' ents' col = searchColumn (tail sch') (map tail ents') col

-- apel recursiv
eval (Select cols q) = eval (Select cols (Atom (eval q)))



eval (SelectLimit cols max (Atom table)) = Table cols (transpose newEnts)
    where
        sch = getSchema table
        ents = getEntries table

        newEnts = map (searchColumnMax sch ents) cols

        searchColumnMax :: TableSchema -> [Entry] -> String -> [String]
        searchColumnMax _ [] col = []
        searchColumnMax [] _ col = []
        searchColumnMax sch' ents' col = if (head sch') == col
                                            then found ents'
                                            else rec sch' ents' col

        found ents' = take (fromIntegral max) (map head ents')
        rec sch' ents' col = searchColumnMax (tail sch') (map tail ents') col

-- apel recursiv
eval (SelectLimit cols max q) = eval (SelectLimit cols max (Atom (eval q)))



eval ((Atom table1) :|| (Atom table2)) = Table sch1 newEnts
    where
        sch1 = getSchema table1
        sch2 = getSchema table2
        ents1 = getEntries table1
        ents2 = getEntries table2

        newEnts = Data.List.union ents1 ents2 -- previne duplicatele

-- apeluri recursive
eval ((Atom table1) :|| q2) = eval ((Atom table1) :|| (Atom (eval q2)))
eval (q1 :|| (Atom table2)) = eval ((Atom (eval q1)) :|| (Atom table2))
eval (q1 :|| q2) = eval ((Atom (eval q1)) :|| (Atom (eval q2)))



eval (Cosine (Atom table)) = Table newSch newEnts
    where
        sch = getSchema table
        -- codul lucreaza cu formatul din baseSch
        ents = getEntries table
        {-ents = if sch == baseSch
                then getEntries table
                else getEntries rearrangedTable

        baseSch = ["user_id", "movie_id", "rating"]
        rearrangedTable = eval (Select baseSch (Atom table))-}

        newSch = ["user_id1", "user_id2", "sim"]
        newEnts = cosAux (sort ents)



-- apel recursiv
eval (Cosine query) = eval (Cosine (Atom (eval query)))



testCosineBaseSch = ["user_id", "movie_id", "rating"]
testCosineSortedEnts = sort (getEntries testCosine1)

testCosine1 = eval $ Filter (Not (Lt "rating" 3)) $ Atom rating
testCosine2 = Table testCosineBaseSch (cosAux testCosineSortedEnts)



testCosine3 = length (cosAux testCosineSortedEnts)


sameUserAux :: [Entry] -> Field -> Float -> [Entry]
sameUserAux [] user sum = [[user, (showFFloat (Just 4) sum "")]]
sameUserAux entries user sum = if name == user
                                then currentEntry : recursiveCall
                                else [[user, (showFFloat (Just 4) sum "")]]
    where
        recursiveCall = sameUserAux (tail entries) user newSum

        newSum = sum + (ratingNum ** 2)
        ratingNum = read (currentEntry !! 2)::Float

        name = head currentEntry
        currentEntry = head entries


testEnts = [["ana", "thriller", "4"], ["ana", "tvd", "10"], ["bob", "tvd", "0"]]
testFunc = sameUserAux testEnts (head (head testEnts))



sameUser :: [Entry] -> [Entry]
sameUser entries = sameUserAux entries user 0
    where
        user = head (head entries)


cosAux :: [Entry] -> [Entry]
cosAux [] = []
cosAux entries = currentStep ++ nextStep
    where
        currentStep = cosCmp userEntries rest
        nextStep = cosAux rest

        rest = drop ((length userEntries) - 1) entries
        userEntries = sameUser entries

ents4 = [["ana", "thriller", "4"], ["ana", "tvd", "10"]] ++ ents3
testCosAux = cosAux ents4

cosCmp :: [Entry] -> [Entry] -> [Entry]
cosCmp uEntries1 [] = []
cosCmp uEntries1 entries = currentStep ++ nextStep
    where
        currentStep = cosCmpU uEntries1 uEntries2 0
        nextStep = cosCmp uEntries1 rest

        rest = drop ((length uEntries2) - 1) entries
        uEntries2 = sameUser entries

ents3 = [["bob", "tvd", "1"], ["bob", "america", "3"], ["clara", "boomer", "4"]]
testCosCmp = cosCmp ents1 ents3


cosCmpU :: [Entry] -> [Entry] -> Float -> [Entry]
cosCmpU uEntries1 uEntries2 pSum = if (length currEntry1) == 2
                                        then evaluate
                                        else nextStep
    where
        evaluate = [[user1, user2, resultString]]

        user1 = currEntry1 !! 0
        user2 = head (head uEntries2)

        resultString = showFFloat (Just 4) result ""
        result = pSum / ((sqrt sum1) * (sqrt sum2))
        sum1 = read (currEntry1 !! 1)::Float
        sum2 = getUserSum uEntries2


        nextStep = cosCmpU (tail uEntries1) uEntries2 pSum'

        pSum' = pSum + (rating1 * rating2)
        rating1 = read (currEntry1 !! 2)::Float
        rating2 = getRating uEntries2 movieID

        movieID = currEntry1 !! 1

        currEntry1 = head uEntries1

ents1 = [["ana", "thriller", "4"], ["ana", "tvd", "10"], ["ana", "116"]]
ents2 = [["bob", "tvd", "1"], ["bob", "america", "3"], ["bob", "10"]]
testCosCmpU = cosCmpU ents1 ents2 0


getRating :: [Entry] -> Field -> Float
getRating uEntries movieID = if (length currEntry) == 2 -- sfarsitul intrarilor
                                then 0
                                else decide
    where
        decide = if (currEntry !! 1) == movieID
                    then read (currEntry !! 2)::Float
                    else getRating (tail uEntries) movieID

        currEntry = head uEntries


getUserSum :: [Entry] -> Float
getUserSum uEntries = if (length currEntry) == 2
                        then sumNum
                        else getUserSum (tail uEntries)
    where
        sumNum = read (currEntry !! 1)::Float
        currEntry = head uEntries


-- TODO 5

same_zone :: String -> Query
same_zone str = Select ["user_id", "occupation"] notUser
    where
        {- dintr-un anume motiv, user-ul dupa care s-a
        cautat zona nu trebuie sa fie in tabela (.ref) -}
        notUser = Filter (Not (Eq "user_id" str)) zone
        zone = Filter (Eq "zone" user_zone) (Atom user_info)
        user_zone = head (head ents)

        (Table schema ents) = eval (Select ["zone"] userID)
        userID = Filter (Eq "user_id" str) (Atom user_info)

male_within_age :: Integer -> Integer -> Query
male_within_age x y = Select ["occupation", "zone"] minAge
    where
        minAge = Filter (Not (Lt "age" (x + 1))) maxAge
        maxAge = Filter (Lt "age" y) males
        males = Filter (Eq "sex" "M") (Atom user_info)

mixed :: [String] -> [String] -> Integer -> Query
mixed zones occupations age = Select ["user_id"] maxAge
    where
        maxAge = Filter (Lt "age" age) occupation
        occupation = Filter (In "occupation" occupations) zone
        zone = Filter (In "zone" zones) (Atom user_info)


