import System.Environment

main = do (fileName:_) <- getArgs  
          content <- readFile fileName
          print(ablauf 0 0 (getTuplets (lines content)) )
          print(ablauf2 0 0 0 (getTuplets (lines content)) )

-- Input in Tupelform bringen (String,Int,Bool)

prefix:: String -> String
prefix x = take 3 x
 
suffix:: String -> Int
suffix x = getNr (drop 4 x)

removePlus:: String -> String
removePlus nr
    | (head nr) == '-' = nr
    | otherwise = tail nr

getNr:: String -> Int
getNr a = read (removePlus a) :: Int

getTuplets:: [String] -> [(String,Int,Bool)]
getTuplets [] = []
getTuplets (x:xs) = ((prefix x),(suffix x), False) : getTuplets xs

-- Tupel verändern
get3rd (_,_,a) = a
get2nd (_,a,_) = a
get1st (a,_,_) = a

setTuplet:: (String,Int,Bool) -> (String,Int,Bool) 
setTuplet (a,b,_) = (a,b,True)

setTupletToJmp:: (String,Int,Bool) -> (String,Int,Bool) 
setTupletToJmp (_,b,c) = ("nop",b,c)

-- Ablauf für Aufgabe 1

ablauf:: Int -> Int -> [(String,Int,Bool)] -> Int
ablauf _ a [] = a
ablauf i a list
    | get3rd (list!!i) = a
    | get1st (list!!i) == "acc" = ablauf (i+1) (a + get2nd (list!!i)) (changeList i list)
    | get1st (list!!i) == "jmp" = ablauf (i+ get2nd (list!!i)) a (changeList i list)
    | otherwise = ablauf (i+1) a (changeList i list)

changeList:: Int -> [(String,Int,Bool)] -> [(String,Int,Bool)]
changeList i list = take i list ++ [setTuplet (list!!i)] ++ drop (i+1) list

changeBez:: Int -> [(String,Int,Bool)] -> [(String,Int,Bool)]
changeBez i list = take i list ++ [setTupletToJmp (list!!i)] ++ drop (i+1) list

ablauf2:: Int -> Int -> Int -> [(String,Int,Bool)] -> Int
ablauf2 i a k list
    | get3rd (list!!i) = ablauf2 (i-k) a 0 (changeBez (i-k) list)
    | length list <= (i+1) && get1st (list!!i) == "acc" = 0
    | length list <= (i+1) = 0
    | get1st (list!!i) == "jmp" = ablauf2 (i+ get2nd (list!!i)) a (get2nd (list!!i)) (changeList i list)
    | get1st (list!!i) == "acc" = ablauf2 (i+1) (a + get2nd (list!!i)) 0 (changeList i list)
    | otherwise = ablauf2 (i+1) a 0 (changeList i list)