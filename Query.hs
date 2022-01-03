{-
PERETE RARES-DANIEL
323CB
PARADIGME DE PROGRAMARE - TEMA 1
-}

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

-- returneaza valoarea din 'entry' care corespunde coloanei 'field' din 'sch'
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

        -- filtru pe entries
        newEnts = filter (getFilter cond sch) ents

-- apel recursiv
eval (Filter cond q) = eval (Filter cond (Atom (eval q)))



eval (Select cols (Atom table)) = Table cols (transpose newEnts)
    where
        sch = getSchema table
        ents = getEntries table

        newEnts = map (searchColumn sch ents) cols

        {- itereaza prin fiecare coloana a tabelului pana cand numele ei
        corespunde cu cel primit -}
        searchColumn :: TableSchema -> [Entry] -> Field -> [Field]
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

        {- similar lui searchColumn de mai sus, doar ca intoarce un numar
        limitat de intrari -}
        searchColumnMax :: TableSchema -> [Entry] -> Field -> [Field]
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
        ents = getEntries table

        sortedEnts = sort ents

        norms = getNorms sortedEnts

        newSch = ["user_id1", "user_id2", "sim"]
        newEnts = cosAux sortedEnts norms



-- apel recursiv
eval (Cosine query) = eval (Cosine (Atom (eval query)))

{- urmatoarele functii incepand de aici pana la urmatorul TODO sunt din
implementarea bonusului -}

{- primeste o lista de intrari si se itereaza prin fiecare user, comparandu-l
pe acesta cu fiecare dintre cei STRICT de dupla el, prin cosCmp -}
cosAux :: [Entry] -> [Float] -> [Entry]
cosAux [] _ = []
cosAux entries norms = currentStep ++ nextStep
    where
        currentStep = cosCmp uEntries1 restEnts norm1 restNorms
        nextStep = cosAux restEnts restNorms

        uEntries1 = sameUser entries
        restEnts = drop (length uEntries1) entries
        

        norm1 = head norms
        restNorms = tail norms

{- primeste o lista de intrari ale unui utilizator si o alta lista de intrari
cu restul utilizatorilor, si compara primul utilizator cu fiecare din a doua
lista -}
cosCmp :: [Entry] -> [Entry] -> Float -> [Float] -> [Entry]
cosCmp uEntries1 [] norm1 norms = []
cosCmp uEntries1 entries norm1 norms = currentStep ++ nextStep
    where
        currentStep = cosCmpU uEntries1 uEntries2 norm1 norm2
        nextStep = cosCmp uEntries1 restEnts norm1 restNorms
        
        uEntries2 = sameUser entries
        restEnts = drop (length uEntries2) entries

        norm2 = head norms
        restNorms = tail norms

{- primeste 2 liste de intrari (fiecare cu cate un utilizator) si creeaza
o noua intrare aferenta cerintei -}
cosCmpU :: [Entry] -> [Entry] -> Float -> Float -> [Entry]
cosCmpU uEntries1 uEntries2 norm1 norm2 = [[user1, user2, resultString]]
    where
        user1 = head (head uEntries1)
        user2 = head (head uEntries2)

        resultString = showFFloat (Just 4) result ""
        result = dotProd / (norm1 * norm2)

        -- produs scalar
        dotProd = getDotProd uEntries1 uEntries2 0

{- primeste o lista de intrari, un user si intoarce primele intrari ce
corespund lui user -}
sameUserAux :: [Entry] -> Field  -> [Entry]
sameUserAux [] user = []
sameUserAux entries user = if name == user
                                then [currentEntry] ++ recursiveCall
                                else []
    where
        recursiveCall = sameUserAux (tail entries) user

        name = head currentEntry
        currentEntry = head entries

{- primeste o lista de intrari si se foloseste de sameUserAux pentru a
returna o alta lista de intrari doar cu primul user, totodata avand la
finalul acesteia valoarea normei calculate -}
sameUser :: [Entry] -> [Entry]
sameUser entries = sameUserAux entries user
    where
        user = head (head entries)

{- primeste o lista de intrari (cu mai multi useri) si returneaza o lista
de norme (ale rating-urilor), cate una pentru fiecare user -}
getNorms :: [Entry] -> [Float]
getNorms [] = []
getNorms entries = [currentStep] ++ nextStep
    where
        currentStep = calcNorm uEntries 0
        nextStep = getNorms rest

        rest = drop (length uEntries) entries
        uEntries = sameUser entries

{- primeste o lista de intrari (toate cu acelasi utilizator) si intoarce 
norma rating-urilor-}
calcNorm :: [Entry] -> Float -> Float
calcNorm [] sum = sqrt sum
calcNorm uEntries sum = calcNorm (tail uEntries) sum'
    where
        rating = (head uEntries) !! 2
        ratingNum = read rating::Float

        sum' = sum + (ratingNum ** 2)

{- primeste 2 liste de intrari (fiecare cu cate un utilizator) si intoarce
produsul scalar al rating-urilor filmelor comune -}
getDotProd :: [Entry] -> [Entry] -> Float -> Float
getDotProd [] uEntries2 sum = sum
getDotProd uEntries1 uEntries2 sum = nextStep
    where
        nextStep = getDotProd restEnts1 uEntries2 newSum

        newSum = sum + (rating1 * rating2)
        rating1 = read (currEntry1 !! 2)::Float
        rating2 = getRating uEntries2 movieID

        movieID = currEntry1 !! 1

        currEntry1 = head uEntries1
        restEnts1 = tail uEntries1

{- primeste o lista de intrari si un ID de film si returneaza rating-ul aferent
acestuia, sau 0 daca nu este gasit -}
getRating :: [Entry] -> Field -> Float
getRating [] movieID = 0
getRating uEntries movieID = if (currEntry !! 1) == movieID
                                then read (currEntry !! 2)::Float
                                else getRating restEnts movieID
    where
        currEntry = head uEntries
        restEnts = tail uEntries

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


