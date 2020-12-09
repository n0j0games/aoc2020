import System.Environment

main = do (fileName:_) <- getArgs  
          content <- readFile fileName
          print(length( getDistinct (checkEachTuplet "shinygold" (getTuplets (lines content)) (getTuplets (lines content)))))
          print(irgendwas "shinygold" (getTuplets2 (lines content)))

-- Allgemein

prefix:: String -> String
prefix x = appendPrefix (take 2 (words x))
 
appendPrefix:: [String] -> String
appendPrefix [] = []
appendPrefix (x:xs) = x ++ appendPrefix xs

getDistinct [] = []
getDistinct (x:xs)
    | isDistinct x xs = x : getDistinct xs
    | otherwise = getDistinct xs

isDistinct _ [] = True
isDistinct a (x:xs)
    | null [x | x <- xs, x == a] = isDistinct a xs
    | otherwise = False 

-- Aufgabe 1

getTuplets:: [String] -> [(String,[String])]
getTuplets [] = []
getTuplets (x:xs) = ((prefix x),(suffix x)) : getTuplets xs

suffix:: String -> [String]
suffix x = appendSuffix (drop 4 (words x))

appendSuffix:: [String] -> [String]
appendSuffix [] = []
appendSuffix (x:y:xs)
    | isNumber x = appendSuffix (y:xs)
    | x == "bags," = appendSuffix (y:xs)
    | x == "bags." = []
    | otherwise = (x ++ y) : appendSuffix xs
appendSuffix (x:xs) = []

isNumber:: String -> Bool
isNumber i
    | null [x | x <- ["0","1","2","3","4","5","6","7","8","9"], x == i] = False
    | otherwise = True

--isInTuplet a=Gesuchter String "shinygold" xs=Suffix, Rückgabe: Bool
isInTuplet:: String -> [String] -> Bool
isInTuplet _ [] = False
isInTuplet a (x:xs)
    | a == x = True
    | otherwise = isInTuplet a xs

--a=Gesuchter String "shinygold" xs=Liste der Tupel, Rückgabe: Liste Prefixe
checkEachTuplet:: String -> [(String,[String])] -> [(String,[String])] -> [String]
checkEachTuplet _ [] _ = []
checkEachTuplet a (x:xs) whole
    | isInTuplet a (snd x) = fst x : checkEachTuplet a xs whole ++ checkEachTuplet (fst x) whole whole
    | otherwise = checkEachTuplet a xs whole

--Aufgabe 2

getTuplets2:: [String] -> [(String,[String])]
getTuplets2 [] = []
getTuplets2 (x:xs) = ((prefix x),(suffix2 x)) : getTuplets2 xs

--Prefix übernehmen

suffix2:: String -> [String]
suffix2 x = appendSuffix2 (drop 4 (words x))

appendSuffix2:: [String] -> [String]
appendSuffix2 [] = []
appendSuffix2 (x:y:z:xs)
    | x == "bags," = appendSuffix2 (y:xs)
    | x == "bags." = []
    | otherwise = (x ++ y ++ z) : appendSuffix2 xs
appendSuffix2 (x:xs) = []

-- a gesuchter Präfix, xs = Liste aller Tupel
irgendwas:: String -> [(String,[String])] -> Int
irgendwas a all = getAmount (findTuplet a all) 

-- a 
--nochwas:: [String] -> [(String,[String])] -> Int
--nochwas [] _ = 0
--nochwas (x:xs) all = irgendwas (fst (findTuplet (tail x) all) all

-- a = gesuchter Präfix, snd [(p,s)] liste aller tupel, rückgabe: liste von strings mit snd des tupels
findTuplet:: String -> [(String,[String])] -> [String]
findTuplet _ [] = []
findTuplet a (x:xs)
    | a == (fst x) = (snd x)
    | otherwise = findTuplet a xs

-- liste gibt anzahl
getAmount:: [String] -> Int
getAmount [] = 0
getAmount (x:xs) = getNr (head x) + getAmount xs

getNr:: Char -> Int
getNr a = read (show a) :: Int   