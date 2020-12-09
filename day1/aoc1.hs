import System.Environment
import System.IO

main = do
    (fileName:_) <- getArgs
    content <- readFile fileName
    print("Solution Day 1 Part 1: " ++ show(mult(find2020Ints(convertToInts content))))
    print("Solution Day 1 Part 2: " ++ show(mult2(find2020Ints2(convertToInts content))))

-- Liest Datei zu [Int]

convertToInts str = [read line::Int | line <- lines str]

--Funktionen für zwei Parameter (Teil A)

find2020Ints:: [Int] -> (Int, Int)
find2020Ints intList = head [(x,y) | (x,y) <- combine intList, x+y==2020]
    where
    combine intList = [(x,y) | x <- intList, y <- intList]

mult:: (Int, Int) -> Int
mult (a, b) = a * b

--Funktionen für drei Parameter (Teil B)

find2020Ints2:: [Int] -> (Int, Int, Int)
find2020Ints2 intList = head [(x,y,z) | (x,y,z) <- combine2 intList, x+y+z==2020]
    where
    combine2 intList = [(x,y,z) | x <- intList, y <- intList, z <- intList]

mult2:: (Int, Int, Int) -> Int
mult2 (a, b, c) = a * b * c
