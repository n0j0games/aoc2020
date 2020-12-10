import System.Environment

main = do (fileName:_) <- getArgs  
          content <- readFile fileName
          print (haupt 0 [] (convertToInt (lines content)))
          print (dasDing (convertToInt (lines content)) (haupt 0 [] (convertToInt (lines content))))

--haupt (x:xs) -> Int, funktion bereits mit gefüllter a-liste und mit xs ohne den ersten 25 elemns aufrufen
haupt:: Int -> [Int] -> [Int] -> Int
haupt _ _ [] = -1
haupt i valid (x:xs)
    | i < 25 = haupt (i+1) (valid ++ [x]) xs
    | checkIt x valid valid = haupt (i+1) (tail valid ++ [x]) xs
    | otherwise = x

checkIt:: Int -> [Int] -> [Int] -> Bool
checkIt _ [] _ = False
checkIt i (a:xs) valid
    | null [x | x <- valid, a+x == i && a /= x] = checkIt i xs valid
    | otherwise = True

convertToInt x =  map (read::String->Int) x :: [Int]

-- Teil 2

dasDing list searched = machtAlles (take (getIndex 0 searched list) list) (take (getIndex 0 searched list) list) searched []

--liste aller vorherigen, gesuchte zahl, liste der genommenen, 
machtAlles:: [Int] -> [Int] -> Int -> [Int] -> Int
machtAlles [] [] _ _ = 0
machtAlles [] all invalid _ = machtAlles (tail all) (tail all) invalid []
machtAlles before all invalid xs
    | sumsToSearchedNr xs invalid = sum [minimum xs,maximum xs]
    | otherwise = machtAlles (tail before) all invalid (xs ++ [head before])

sumsToSearchedNr:: [Int] -> Int -> Bool
sumsToSearchedNr a x
    | sum a == x = True
    | otherwise = False

getIndex _ _ [] = -1
getIndex i searched (x:xs)
    | searched == x = i
    | otherwise = getIndex (i+1) searched xs

