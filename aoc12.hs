import System.Environment
main = do (fileName:_) <- getArgs  
          content <- readFile fileName
          print (start2 content)

-- Abgeändert aus aoc8

getTuplets:: [String] -> [(Char,Int)]
getTuplets [] = []
getTuplets (x:xs) = ((prefix x),(suffix x)) : getTuplets xs

prefix:: String -> Char
prefix x = head x
 
suffix:: String -> Int
suffix x = read (drop 1 x) :: Int

-- Char c = Current direction
-- Rotating direction, d = Degree
rotating:: Char -> Int -> Char
rotating c d
    | c == 'N' && d == 90 = 'E'
    | c == 'N' && d == 180 = 'S'
    | c == 'N' && d == 270 = 'W'
    | c == 'E' && d == 90 = 'S'
    | c == 'E' && d == 180 = 'W'
    | c == 'E' && d == 270 = 'N'
    | c == 'S' && d == 90 = 'W'
    | c == 'S' && d == 180 = 'N'
    | c == 'S' && d == 270 = 'E'
    | c == 'W' && d == 90 = 'N'
    | c == 'W' && d == 180 = 'E'
    | c == 'W' && d == 270 = 'S'
    | d == 0 = c
    | d == 360 = c
    | otherwise = error "Falsche Gradzahl"

-- Reverse, da rotating nur für rechts geht
reverseRotating:: Int -> Int
reverseRotating d = 360 - d

-- doTasks, c=current, (N,E), x:xs, ausgabe (N,E)
doTasks:: Char -> (Int,Int) -> [(Char,Int)] -> (Int,Int)
doTasks _ a [] = a
doTasks c a (x:xs)
    | elem (fst x) ['N','E','S','W'] = doTasks c (move (fst x) (snd x) a) xs
    | (fst x) == 'F' = doTasks c (move c (snd x) a) xs
    | (fst x) == 'L' = doTasks (rotating c (reverseRotating (snd x))) a xs
    | (fst x) == 'R' = doTasks (rotating c (snd x)) a xs
    | otherwise = error "Existiert nicht"

getFinalDestination content = doTasks 'E' (0,0) (getTuplets (lines content))

start content = (abs (fst (getFinalDestination content))) + (abs (snd (getFinalDestination content)))

-- doTasks, c=current, (N,E), (X,Y)=Waypoint, x:xs, ausgabe (N,E),
doTasks2:: (Int,Int) -> (Int,Int) -> [(Char,Int)] -> (Int,Int)
doTasks2 a _ [] = a
doTasks2 a w (x:xs)
    | elem (fst x) ['N','E','S','W'] = doTasks2 a (move (fst x) (snd x) w) xs
    | (fst x) == 'L' = doTasks2 a (leftRotateWP w (snd x)) xs
    | (fst x) == 'R' = doTasks2 a (rotateWP w (snd x)) xs 
    | (fst x) == 'F' = doTasks2 (forwarding (snd x) a w) w xs
    | otherwise = error "Existiert nicht II"

getFinalDestination2 content = doTasks2 (0,0) (1,10) (getTuplets (lines content))
start2 content = abs (fst (getFinalDestination2 content)) + abs (snd (getFinalDestination2 content))

rotateWP:: (Int,Int) -> Int -> (Int,Int)
rotateWP a d
    | d == 90 = (-y,x)
    | d == 180 = (-x,-y)
    | d == 270 = (y,-x)
    | otherwise = error "Arschloch" 
        where
            x = fst a
            y = snd a

leftRotateWP a d = rotateWP a (360 - d)

--forwarding i a w
forwarding:: Int -> (Int,Int) -> (Int,Int) -> (Int,Int)
forwarding i a w = (shipX+wpX*i, shipY+wpY*i) 
    where
        shipX = fst a
        shipY = snd a
        wpX = fst w
        wpY = snd w 

-- Move erhält die Richtung, und die Stecke die er geht, erhält ein Tupel (N,E), gibt dies zurück
move:: Char -> Int -> (Int,Int) -> (Int,Int)
move c l a
    | c == 'N' = ( (fst a)+l, (snd a) )
    | c == 'S' = ( (fst a)-l, (snd a) )
    | c == 'E' = ( (fst a), (snd a)+l )
    | c == 'W' = ( (fst a), (snd a)-l )
    | otherwise = error "Richtung existiert nicht"