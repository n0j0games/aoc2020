import System.Environment

main = do
    (fileName:_) <- getArgs
    content <- readFile fileName
    print (checkPPs content)

-- split all lines

convertToPassport:: [String] -> [[String]]
convertToPassport linesList = splitLinesToPassports linesList (findEmptyLines linesList)

filterTrues xs = [x | x <- xs,x == True]

checkPPs content = (checkPP(words(combinePPLines(head(convertToPassport (lines content))))))

findEmptyLines linesList = [nr | (line, nr) <- addLineNr linesList, line == ""]

addLineNr linesList = zip linesList [0..(length linesList - 1)]

splitLinesToPassports linesList [pos1] = [drop (pos1+1) linesList]
splitLinesToPassports linesList (pos1:pos2:restPos) = drop (pos1+1) (take pos2 linesList) : splitLinesToPassports linesList (pos2:restPos)

combinePPLines [] = []
combinePPLines (x:xs) = x++" "++combinePPLines xs

checkPP pp = checkvalids (length valids) valids (removeCID(pp)) (removeCID(pp)) == 7

removeCID pp = [x| x <- pp, "cid:" /= take 4 x]

valids = ["iyr","eyr","hgt","hcl","ecl","pid","byr"]

checkvalids:: Int -> [String] -> [String] -> [String] -> Int
checkvalids 0 _ _ _ = 0
checkvalids _ _ _ [] = 0
checkvalids i valids start (x:xs)
        | valids!!(i-1) == take 3 x = 1 + (checkvalids (i-1) valids start start)
        | otherwise = checkvalids i valids start (xs)
        
checkTheOtherOne i valids xs
        | i==0 = isinNumbers 

isinNumbers xs min max
        | min <= xs && xs <= max = True
        | otherwise = False