import System.Environment

main = do (fileName:_) <- getArgs  
          content <- readFile fileName
          print( nr1 content)
          print(hasEveryone (head(head(convertToGroups (lines content)))) (head(tail(convertToGroups (lines content)))))

-- General

convertToGroups :: [String] -> [[String]]
convertToGroups linesList = take (head (findEmptyLines linesList)) linesList : splitLinesToGroups linesList (findEmptyLines linesList)

splitLinesToGroups linesList [pos1]                = [drop (pos1+1) linesList]
splitLinesToGroups linesList (pos1:pos2:restPos)   = drop (pos1+1) (take pos2 linesList) : splitLinesToGroups linesList (pos2:restPos)

findEmptyLines linesList = [nr | (line, nr) <- addLineNr linesList, line == ""]

addLineNr linesList = zip linesList [0..(length linesList - 1)]

--Aufgabe 1

nr1 xs = getAmount (getDistincts (splitToGroup (convertToGroups (lines xs))))

splitToGroup:: [[String]] -> [String]
splitToGroup [] = []
splitToGroup (x:xs) = mergeTheGroup x : splitToGroup xs

mergeTheGroup:: [String] -> String
mergeTheGroup [] = []
mergeTheGroup (x:xs) = x ++ x ++ mergeTheGroup xs

getAmount [] = 0
getAmount (x:xs) = length x + getAmount xs

getDistincts:: [String] -> [String]
getDistincts [] = []
getDistincts (x:xs) = distinct [] x : getDistincts xs

distinct:: String -> String -> String
distinct ys [] = ys
distinct ys (x:xs)
    | isPart x ys = distinct ys xs
    | otherwise = distinct (x:ys) xs 

isPart a ys
    | null [x | x <- ys, x == a] = False
    | otherwise = True 

-- Aufgabe 2

--hasEveryone String (erste Zeile) [String] restliche Zeilen -> erste Zeile
hasEveryone:: String -> [String] -> [String]
hasEveryone [] _ = []
hasEveryone (x:xs) rest
    | isInAnyOther x rest = [x] : hasEveryone xs rest
    | otherwise = hasEveryone xs rest

-- Char [String]
isInAnyOther:: Char -> [String] -> Bool
isInAnyOther _ [] = True
isInAnyOther a ((i:x):xs)
    | null [i | i <- x, i == a] = False
    | otherwise = isInAnyOther a xs