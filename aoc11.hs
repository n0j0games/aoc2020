import System.Environment

main = do (fileName:_) <- getArgs  
          content <- readFile fileName
          print ("Solution Day 11 Part 1: "++show (nr1 $ lines content))
          print ("Solution Day 11 Part 2: "++show (nr2 $ lines content))


nr2 input
    | seatsAfterChange == input = length $ filter ('#'==) (concat input)
    | otherwise                 = nr2 seatsAfterChange
    where seatsAfterChange = changeSeatsAll input 2

nr1 input
    | seatsAfterChange == input = length $ filter ('#'==) (concat input)
    | otherwise                 = nr1 seatsAfterChange
    where seatsAfterChange = changeSeatsAll input 1

changeSeatsAll input = changeSeatsRows input 0 input input

changeSeatsRows []             _     _     output _       = output
changeSeatsRows (row:restRows) rowNr input output aufgabe = changeSeatsRows restRows (rowNr+1) input outputAfterChange aufgabe
    where outputAfterChange = changeSeats row (0, rowNr) input output aufgabe


changeSeats []     _              _     output _       = output
changeSeats (x:xs) (seatX, seatY) input output aufgabe = changeSeats xs (seatX+1, seatY) input outputAfterChange aufgabe
    where outputAfterChange = changeSeatState x (seatX, seatY) input output aufgabe

changeSeatState seatChar seatNr input output aufgabe
    | seatChar == '.'                 = output
    | seatChar == 'L'                 = if getAdjacentOccSeats seatNr input aufgabe == 0 then changeSeatState2 seatNr output '#' else output
    | seatChar == '#' && aufgabe == 1 = if getAdjacentOccSeats seatNr input aufgabe >= 4 then changeSeatState2 seatNr output 'L' else output
    | seatChar == '#' && aufgabe == 2 = if getAdjacentOccSeats seatNr input aufgabe >= 5 then changeSeatState2 seatNr output 'L' else output

changeSeatState2 (seatX, seatY) input char = take seatY input ++ [take seatX row ++[char]++drop (seatX+1) row]++drop (seatY+1) input
    where row = input!!seatY


getAdjacentOccSeats seatNr input aufgabe
    | aufgabe == 1 = getAdjacentOccSeats1 seatNr input
    | aufgabe == 2 = getAdjacentOccSeats2 seatNr input

getSeat (seatX, seatY) input
    | 0 > seatY || 0 > seatX = 'L'
    | length input > seatY  && length (input!!seatY) > seatX = input!!seatY!!seatX
    | otherwise = 'L'

getAdjacentOccSeats1 (seatX, seatY) input = length $ filter ('#'==) [getSeat (seatX-1, seatY-1) input, getSeat (seatX, seatY-1) input, getSeat (seatX+1, seatY-1) input,
                                                                     getSeat (seatX-1, seatY) input,                                   getSeat (seatX+1, seatY) input,
                                                                     getSeat (seatX-1, seatY+1) input, getSeat (seatX, seatY+1) input, getSeat (seatX+1, seatY+1) input]

getAdjacentOccSeats2 seatNr input = getNWSeat seatNr input + getNSeat seatNr input +getNESeat seatNr input +getESeat seatNr input +getSESeat seatNr input +getSSeat seatNr input +getSWSeat seatNr input +getWSeat seatNr input

getNWSeat (seatX, seatY) input
    | getSeat (seatX-1, seatY-1) input == 'L' = 0
    | getSeat (seatX-1, seatY-1) input == '#' = 1
    | otherwise                               = getNWSeat (seatX-1, seatY-1) input

getNSeat (seatX, seatY) input
    | getSeat (seatX, seatY-1) input == 'L' = 0
    | getSeat (seatX, seatY-1) input == '#' = 1
    | otherwise                             = getNSeat (seatX, seatY-1) input


getNESeat (seatX, seatY) input
    | getSeat (seatX+1, seatY-1) input == 'L' = 0
    | getSeat (seatX+1, seatY-1) input == '#' = 1
    | otherwise                               = getNESeat (seatX+1, seatY-1) input


getESeat (seatX, seatY) input
    | getSeat (seatX+1, seatY) input == 'L' = 0
    | getSeat (seatX+1, seatY) input == '#' = 1
    | otherwise                             = getESeat (seatX+1, seatY) input


getSESeat (seatX, seatY) input
    | getSeat (seatX+1, seatY+1) input == 'L' = 0
    | getSeat (seatX+1, seatY+1) input == '#' = 1
    | otherwise                               = getSESeat (seatX+1, seatY+1) input

getSSeat (seatX, seatY) input
    | getSeat (seatX, seatY+1) input == 'L' = 0
    | getSeat (seatX, seatY+1) input == '#' = 1
    | otherwise                             = getSSeat (seatX, seatY+1) input

getSWSeat (seatX, seatY) input
    | getSeat (seatX-1, seatY+1) input == 'L' = 0
    | getSeat (seatX-1, seatY+1) input == '#' = 1
    | otherwise                               = getSWSeat (seatX-1, seatY+1) input

getWSeat (seatX, seatY) input
    | getSeat (seatX-1, seatY) input == 'L' = 0
    | getSeat (seatX-1, seatY) input == '#' = 1
    | otherwise                             = getWSeat (seatX-1, seatY) input