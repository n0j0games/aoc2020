import System.Environment  
import System.IO  
  
main = do (fileName:_) <- getArgs  
          content <- readFile fileName
          print("Solution Day 1 Part 1: "++show (findValidCount (convertLines (lines content))))
          print("Solution Day 1 Part 2: "++show (findValidCount2 (convertLines (lines content))))


convertLines:: [String] -> [(Int, Int, Char, String)]
convertLines linesList = [convertLine l | l <- linesList]

convertLine:: String -> (Int, Int, Char, String)
convertLine line = (fst (getLimits line), snd (getLimits line), (words line)!!1!!0, (words line)!!2)


getLimits:: String -> (Int, Int)
getLimits string = (read (getLimit string)::Int, read(getLimit (drop ((length (getLimit string)+1)) string))::Int)
    where
    getLimit (c:rest)
        | c == '-' = []
        | c == ' ' = []
        | otherwise = (c:(getLimit rest))


countCharInString:: Char -> String -> Int
countCharInString c string = length [x | x <- string, x==c]

findValids:: [(Int, Int, Char, String)] -> [Bool]
findValids tuples = [limitMin <= (countCharInString c s) && (countCharInString c s) <= limitMax | (limitMin, limitMax, c, s) <- tuples]

findValidCount:: [(Int, Int, Char, String)] -> Int
findValidCount tuples = length [bool | bool <- findValids tuples, bool]

--Aufgabe 2

findValidCount2 :: [(Int, Int, Char, String)] -> Int
findValidCount2 tuples = length [bool | bool <- findValids2 tuples, bool]

findValids2 :: [(Int, Int, Char, String)] -> [Bool]
findValids2 tuples = [if (length s >= second-1) then (c == s!!(first-1)) /= (c == s!!(second-1)) else False | (first, second, c, s) <- tuples]