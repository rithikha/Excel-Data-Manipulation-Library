
module Localparser where

import System.Exit

-- splitsep (==',') "3,5,"  => ["3","5",""]
-- splitsep delimiter ignore list
splitsep _ _ [] = [[]]
splitsep sep ig (h:t)
    | sep h = []: splitsep sep ig t
    | ig h = splitsep sep ig t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep ig t


--go :: IO (a, Int) type of the function go??
go = beginprogram "HateCrimesByRegion2016.csv"


fls x = False
beginprogram filename =
  do
    putStrLn "The following functions are available: \n \n 1.  select_row \n 2.  select_column \n 3.  sum_row \n 4.  sum_column \n 5.  average_row \n 6.  average_column \n 7.  min_row \n 8.  max_row \n 9.  min_column \n 10. max_column \n 11. select_value \n 12. read_int \n 13. row_name \n 14. column_name \n 15. row_column_name \n 16. compare_values \n 17. compare_columns \n 18. compare_rows \n To start please type in the function and its corresponding parameters"
    file <- readFile filename
    let elems = [splitsep (==',') (`elem` "\160\r\65279") line| line <- splitsep (=='\n') fls file]
    func <- getLine
    let commands = splitsep (==' ') (`elem` "") func
    let res = lookupfun commands elems
    return res


lookupfun :: [String] -> [[String]] -> String
lookupfun args dataset
    | args == "select_row":(tail args) = show (select_row (read (head (tail args))) dataset)
    | args == "select_column":(tail args) = show (select_column (read (head (tail args))) dataset)
    | args == "sum_row":(tail args) = show (sum_row (read (head (tail args))) dataset)
    | args == "sum_column":(tail args) = show (sum_column (read (head (tail args))) dataset)
    | args == "average_row":(tail args) = show (average_row (read (head (tail args))) dataset)
    | args == "average_column":(tail args) = show (average_column (read (head (tail args))) dataset)
    | args == "min_row":(tail args) = show (min_row (read (head (tail args))) dataset)
    | args == "max_row":(tail args) = show (max_row (read (head (tail args))) dataset)
    | args == "min_column":(tail args) = show (min_column (read (head (tail args))) dataset)
    | args == "max_column":(tail args) = show (max_column (read (head (tail args))) dataset)
    | args == "select_value":(tail args) = show (select_value (read (head (tail args))) dataset) --gives weird extar \'s and '
    | args == "row_name":(tail args) = show (row_name (read (head (tail args))) dataset)
    | args == "column_name":(tail args) = show (column_name (read (head (tail args))) dataset)
    | args == "row_column_name":(tail args) = show (row_column_name (read (head (tail args))) dataset)
    | args == "compare_values":(tail args) = show (compare_values (read (head (tail args))) (read (head (tail (tail args)))) dataset)
    | args == "compare_columns":(tail args) = show (compare_columns (read (head (tail args))) (read (head (tail (tail args)))) (read (head (tail (tail (tail args))))) dataset)
    | args == "compare_rows":(tail args) = show (compare_rows (read (head (tail args))) (read (head (tail (tail args)))) (read (head (tail (tail (tail args))))) dataset)
    | otherwise = "Sorry, that is an invalid input. Please try again"





---------------------------------------------------- 

--Data Functions:

-- returns row r in the CSV
select_row :: Int -> [a] -> a
select_row r elems = elems !! r

-- returns columns r in the CSV
select_column :: Int -> [[a]] -> [a]
select_column r [] = []
select_column r (h:t) = (h !! r):select_column r t  

-- returns the sum of row r in the CSV
sum_row :: Int -> [[String]] -> Int
sum_row r elems = sum [read e :: Int | e <- tail (elems !! r)]

-- returns the sum of column r in the CSV
sum_column :: Int -> [[String]] -> Int
sum_column r elems = sum [read e :: Int | e <- tail (select_column r elems)]

-- returns the average of row r in the CSV
average_row :: Fractional a => Int -> [[String]] -> a
average_row r elems = (fromIntegral (sum_row r elems)) / (fromIntegral ((length (head elems)) - 1))

-- returns the sum of column r in the CSV
average_column :: Fractional a => Int -> [[String]] -> a
average_column r elems = (fromIntegral (sum_column r elems)) / (fromIntegral ((length (select_column r elems)) - 1))

-- returns the minimum value from the selected row
min_row :: Int -> [[String]] -> Int
min_row r elems = minimum (map (\ x ->  read x :: Int) (tail (select_row r elems)))

-- returns the maximum value from the selected row
max_row :: Int -> [[String]] -> Int
max_row r elems = maximum (map (\ x ->  read x :: Int) (tail (select_row r elems)))

-- returns the minimum value from the selected column
min_column :: Int -> [[String]] -> Int
min_column r elems = minimum (map (\ x ->  read x :: Int) (tail (select_column r elems)))

-- returns the maximum value from the selected column
max_column :: Int -> [[String]] -> Int
max_column r elems = maximum (map (\ x ->  read x :: Int) (tail (select_column r elems)))

-- returns value in row r and column c as Char
select_value :: (Int, Int) -> [[[Char]]] -> [Char]
select_value (r,c) elems = (select_row r elems) !! c

-- returns value in row r and column c by casting the Char to type Int
read_int :: (Int, Int) -> [[[Char]]] -> Int
read_int (r,c) elems = read ((select_row r elems) !! c)

-- given a cell index returns the row header
row_name :: (Int, Int) -> [[a]] -> a
row_name (r,c) elems = (select_row r elems) !! 0

--given a cell index returns the column header
column_name :: (Int, Int) -> [[a2]] -> a2
column_name (r,c) elems = (select_column c elems) !! 0

--given a cell index returns the row header and column header as a pair 
row_column_name :: (Int, Int) -> [[b]] -> (b, b)
row_column_name (r,c) elems = zip [(select_row r elems) !! 0] [(select_column c elems) !! 0] !! 0                                        

--compare any two cell values and returns the coordinates of the greater value 
compare_values :: (Int, Int) -> (Int, Int) -> [[[Char]]] -> (Int, Int)
compare_values (r1,c1) (r2,c2) elems = (if (read_int (r1,c1) elems) > (read_int (r2,c2) elems) then (r1,c1) else (r2,c2))

-- compares two column values given a specific row, and returns the corresponding header
compare_columns :: Int -> Int -> Int -> [[[Char]]] -> [Char]
compare_columns c1 c2 r elems = column_name (compare_values (r,c1) (r,c2) elems) elems

-- compares two row values given a specific column, and returns the corresponding header
compare_rows :: Int -> Int -> Int -> [[[Char]]] -> [Char]
compare_rows r1 r2 c elems = row_name (compare_values (r1,c) (r2,c) elems) elems

---------------

--user  = average_columns 3
--user = select_row 1
--user = select_column 0
--user = select_value (2,4)
--user = read_value (2,4)
--user = compare_values (1,1) (2,2)
--user = row_column_name (1,1)
--user = compare_values (2,2) (2,3)
--user = compare_columns 2 3 4
--user = column_name (0,8)
--user = max_column 1
