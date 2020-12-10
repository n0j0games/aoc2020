import System.Environment

main = do (fileName:_) <- getArgs  
          content <- readFile fileName
          print ("Solution Day 6 Part 1: "++show (nr1 content))
          print ("Solution Day 6 Part 2: "++show (nr2 content))


nr2 content = length $ getChilds2 (convertChilds $ getChilds ["shiny", "gold"] bagList) bagList
    where bagList = map ruleToBag (lines content)


getChilds2 :: [[String]] -> [([String], [String])] -> [[String]]
getChilds2 [] _ =  []
getChilds2 (x:xs) bagList = x : getChilds2 (convertChilds $ getChilds x bagList) bagList ++ getChilds2 xs bagList

convertChilds [] = []
convertChilds (count:color1:color2:_:xs) = multiplyChild (read count::Int) [color1, color2] ++ convertChilds xs
convertChilds ["no", "other", "bags."] = []

multiplyChild :: Int -> [String] -> [[String]]
multiplyChild 0     _                = []
multiplyChild count [color1, color2] = [color1, color2] : multiplyChild (count-1) [color1, color2] 

getChilds :: [String] -> [([String], [String])] -> [String]
getChilds _                [] = []
getChilds [color1, color2] (bag:bagList)
    | isAncestor [color1, color2] bag = snd bag
    | otherwise                       = getChilds [color1, color2] bagList

isAncestor [color1, color2] bag = [color1, color2] == fst bag

nr1 content = length $ getDistinct $ getAncestors2 (getAncestors ["shiny", "gold"] bagList) bagList
    where bagList = map ruleToBag (lines content)



getDistinct [] = []
getDistinct (x:xs)
    | isInList x xs =     getDistinct xs
    | otherwise     = x : getDistinct xs

isInList _ [] = False
isInList x (y:ys)
    | x == y    = True
    | otherwise = isInList x ys

getAncestors2 :: [[String]] -> [([String], [String])] -> [[String]]
getAncestors2 [] _ = []
getAncestors2 (x:xs) bagList = x : getAncestors2 (getAncestors x bagList) bagList ++ getAncestors2 xs bagList

ruleToBag x = (take 2 (words x), drop 4 (words x))


getAncestors :: [String] -> [([String], [String])] -> [[String]]
getAncestors _                []         = []
getAncestors [color1, color2] (bag:bagList)
    | isChild [color1, color2] (snd bag) = fst bag : getAncestors [color1, color2] bagList
    | otherwise                          = getAncestors [color1, color2] bagList


isChild _ [_] = False
isChild [color1, color2] (childPart1:childPart2:childs)
    | color1 == childPart1 && color2 == childPart2 = True
    | otherwise = isChild [color1, color2] (childPart2:childs)