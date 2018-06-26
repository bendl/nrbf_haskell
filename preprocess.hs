-- Gridwatch download preprocessor

import System.Environment
import Data.List
import Data.List.Split

import Debug.Trace
trace' arg = trace (show arg) arg

normalise_date (d:m:h:[]) = [d/31, m/12, h/24]

-- Round f to n decimal places
dp2 f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

parse_line :: String -> [Double]
parse_line line = do
    let parts       = splitOn "," line
        date_time   = splitOn " " (parts !! 1)
        dates       = splitOn "-" (date_time !! 1)
        times       = splitOn ":" (date_time !! 2)
        
        demand  = (read (parts !! 2) :: Double)
        month   = (read (dates !! 1) :: Double)
        day     = (read (dates !! 2) :: Double)
        hour    = (read (times !! 0) :: Double)

        row = (map (\f -> dp2 f 2) $
            (normalise_date [day, month, hour])) ++ [demand]
        in
            row

d :: [Int]
d = [1,1,2,3,1,1,2,2,3]
d2 :: [[Double]]
d2 = [[0, 0, 0, 0], [0,0,0,0], [0,0,1,0], [0,0,2,0], [0,0,1,0]]

-- getUnique' (not equal to last)
gu' :: Int -> [Int] -> [Int]
gu' h [] = [h]
gu' h t = 
    case (h == (head t)) of
        True    -> gu' (head t) (tail t)
        False   -> [h] ++ (gu' (head t) (tail t))

-- getUnique (not equal to last)
gu :: [Int] -> [Int]
gu row = gu' (head row) (tail row)

gu2' :: [Double] -> [[Double]] -> [[Double]]
gu2' h [] = [h]
gu2' h t =
    case ((h !! 2) == ((head t) !! 2)) of
        True    -> gu2' (head t) (tail t)
        False   -> [h] ++ (gu2' (head t) (tail t))

gu2 :: [[Double]] -> [[Double]]
gu2 row = gu2' (head row) (tail row)

unique_hour' :: [[Double]] -> Double -> [[Double]]
unique_hour' [] _ = []
unique_hour' rows hour = do
    let result = (filter (\[d,m,h,de] -> h /= hour) (rows))
        in result ++ (unique_hour' result (hour + 1))

unique_hour :: [[Double]] -> [[Double]]
unique_hour rows = do
    [[0]]

-- :main gridwatch.2013-2014.csv
main :: IO()
main = do
    args  <- getArgs
    filec <- readFile $ args !! 0
    
    let 
        file_name   = args !! 0
        file_lines  = lines filec
        header      = take 1 file_lines
        file_data   = drop 1 file_lines
        test_data   = file_data
        out_data    = map parse_line test_data
        out_data_s  = unlines $ map show out_data

        in do
            -- print out first 10 data lines
            putStrLn $ unlines $ map show $ take 10 out_data
            
            -- Write normalised training data to file
            --writeFile (file_name ++ ".out.csv") out_data_s