import System.Environment  
  
main = do (fileName:_) <- getArgs  
          content <- readFile fileName
          print ("Solution Day 3 Part 1: "++ show (slide (lines content) (1, 1) (3, 1)))
          print ("Solution Day 3 Part 2: "++ show (multy content))


slide:: [String] -> (Int, Int) -> (Int, Int) -> Int
slide linesList (x, y) (right, down)
    | length linesList < y              = 0
    | length (linesList!!(y-1)) < x     =     slide (duplicateLine linesList (y-1)) (x, y) (right, down)
    | linesList!!(y-1)!!(x-1) == '.'    =     slide linesList (x+right, y+down) (right, down)
    | linesList!!(y-1)!!(x-1) == '#'    = 1 + slide linesList (x+right, y+down) (right, down)



duplicateLine:: [String] -> Int -> [String]
duplicateLine linesList y = take y linesList ++ [linesList!!y ++ linesList!!y] ++ drop (y+1) linesList

multy m = (slide (lines m) (1, 1) (1, 1)) * (slide (lines m) (1, 1) (3, 1)) * (slide (lines m) (1, 1) (5, 1)) * (slide (lines m) (1, 1) (7, 1)) * (slide (lines m) (1, 1) (1, 2))