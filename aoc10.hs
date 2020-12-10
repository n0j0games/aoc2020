import System.Environment
import Data.List

main = do (fileName:_) <- getArgs  
          content <- readFile fileName
          print (getDiffs 0 0 0 0 (convertToInt (lines content)))
          print ("Missing Part 2")

convertToInt x =  map (read::String->Int) x :: [Int]

--get the next 1st, 2nd, 3rd number from the List
getNumberFromList:: Int -> Int -> [Int] -> Int
getNumberFromList _ _ [] = -1
getNumberFromList g s (x:xs)
    | (g+s) == x = x
    | otherwise = getNumberFromList g s xs

--calculate the shortest difference
getDiff:: Int -> [Int] -> Int
getDiff g xs
    | getNumberFromList g 1 xs /= -1 = 1
    | getNumberFromList g 2 xs /= -1 = 2
    | getNumberFromList g 3 xs /= -1 = 3
    | otherwise = -1

--get any difference
getDiffs:: Int -> Int -> Int -> Int -> [Int] -> Int
getDiffs c a1 a2 a3 xs 
    | getDiff c xs == 1 = getDiffs (c+1) (a1 + 1) a2 a3 xs
    | getDiff c xs == 2 = getDiffs (c+2) a1 (a2 + 1) a3 xs
    | getDiff c xs == 3 = getDiffs (c+3) a1 a2 (a3 + 1) xs
    | otherwise = a1 * (a3+1)

-- Part 2

possibs:: Int -> [Int] -> Int
possibs a xs
    | len == 1 = possibs (a+first) (drop 1 xs)
    | len == 2 = possibs (a+first) (drop 1 xs) + possibs (a+second) (drop 2 xs)
    | len == 3 = possibs (a+first) (drop 1 xs) + possibs (a+second) (drop 2 xs) + possibs (a+third) (drop 3 xs)
    | otherwise = 1
    where
        len = length (getAllNumbers a xs) 
        first = getAllNumbers a xs !! 0
        second = getAllNumbers a xs !! 1
        third = getAllNumbers a xs !! 2

getAllNumbers:: Int -> [Int] -> [Int]
getAllNumbers g xs
    | (getNumberFromList g 3 xs /= -1) && (getNumberFromList g 2 xs /= -1) && (getNumberFromList g 1 xs /= -1)= [1,2,3]
    | (getNumberFromList g 1 xs /= -1) && (getNumberFromList g 2 xs /= -1) = [1,2]
    | (getNumberFromList g 2 xs /= -1) && (getNumberFromList g 3 xs /= -1) = [2,3]
    | (getNumberFromList g 1 xs /= -1) && (getNumberFromList g 3 xs /= -1) = [1,3]
    | getNumberFromList g 1 xs /= -1 = [1]
    | getNumberFromList g 2 xs /= -1 = [2]
    | getNumberFromList g 3 xs /= -1 = [3]
    | otherwise = []
