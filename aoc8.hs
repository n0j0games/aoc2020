import System.Environment

main = do (fileName:_) <- getArgs  
          content <- readFile fileName
          print ("Solution Day 8 Part 1: "++show (runCode (linesWithNumber $ lines content) [] 0 0))
          print ("Solution Day 8 Part 2: "++show (runCode2 (map linesWithNumber (changeCode (lines content)))))

runCode2 (code:codeList)
    | snd (runCode code [] 0 0) = fst (runCode code [] 0 0)
    | otherwise                 = runCode2 codeList

changeCode :: [String] -> [[String]]
changeCode linesList = changeToJmp++changeToNop
    where
        changeToNop = [changeLine "nop" linesList i | i <- [0..(length linesList-1)], take 3 (linesList!!i) == "jmp"]
        changeToJmp = [changeLine "jmp" linesList i | i <- [0..(length linesList-1)], take 3 (linesList!!i) == "nop"]
        changeLine cmd linesList i = take i linesList ++ [cmd++drop 3 (linesList!!i)] ++ drop (i+1) linesList

runCode linesList visitedLines nextLine acc
    | length linesList <= nextLine                 = (acc, True)
    | isInList nextLine visitedLines              = (acc, False)
    | take 3 (fst (linesList!!nextLine)) == "nop" = runCode linesList (snd (linesList!!nextLine):visitedLines) (nextLine+1) acc
    | take 3 (fst (linesList!!nextLine)) == "acc" = runCode linesList (snd (linesList!!nextLine):visitedLines) (nextLine+1) (acc+getArgument (fst (linesList!!nextLine)))
    | take 3 (fst (linesList!!nextLine)) == "jmp" = runCode linesList (snd (linesList!!nextLine):visitedLines) (nextLine + getArgument (fst (linesList!!nextLine))) acc

getArgument :: String -> Int
getArgument line
    | line!!4 == '+' = read (drop 5 line)::Int
    | otherwise      = -1*read (drop 5 line)::Int

linesWithNumber linesList = zip linesList [0..(length linesList)]

isInList _   [] = False
isInList var (x:xs)
    | var == x  = True
    | otherwise = isInList var xs