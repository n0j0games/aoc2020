import System.Environment

main = do
    (fileName:_) <- getArgs
    content <- readFile fileName
    print(getHighestTicketNr content)
    print(filterIt (listSeats (getHighestTicketNr content) (doForEach(lines content))) (getHighestTicketNr content))
    --print("Row: " ++ show (binReader 0 127 0 (last(lines (content)))) ++ " , Column: " ++ show (binReader 0 7 0 (getRestOfList (last(lines (content))))) ++ " , Solution: " ++ show (getRowAndColumn (last(lines (content)))))

getHighestTicketNr xs = maximum ( doForEach (lines xs))
--rowReader int:min int:max int:index string:xs, return int:nr
binReader:: Int -> Int -> Int -> String -> Int
binReader min max i (x:xs)
    | abs (min - max) <= 1 && (x == 'F' || x == 'L') = min
    | abs (min - max) <= 1 && (x == 'B' || x == 'R') = max
    | x == 'F' || x == 'L' = binReader min (getNewBorder min max) (i+1) xs
    | otherwise = binReader (getNewBorder min max +1) max (i+1) xs

getNewBorder min max = min + (max - min) `div` 2

getRowAndColumn xs = binReader 0 127 0 xs * 8 + binReader 0 7 0 (getRestOfList xs)

getRestOfList = drop 7 

doForEach:: [String] -> [Int]
doForEach = map getRowAndColumn

getSeat a xs = [x | x <- xs, x == a]
--ggf den wert 904 anders herziehen

listSeats (-1) _ = []
listSeats i xs
    |  null (getSeat i xs) = i : listSeats (i-1) xs
    |  otherwise = listSeats (i-1) xs

-- xs: die liste aus listSeats, aber reversed, da diese falschrum ist
filterIt:: [Int] -> Int -> Int
filterIt xs max = head (filterLast max (filterFirst 0 (reverse xs)))

--startet von 0
filterFirst:: Int -> [Int] -> [Int]
filterFirst i xs
    | head xs == i = filterFirst (i+1) (tail xs)
    | otherwise = xs

--erhält die liste von filterFirst, startet beim maximum
filterLast:: Int -> [Int] -> [Int]
filterLast i xs
    | last xs == i = filterLast (i-1) (init xs)
    | otherwise = xs