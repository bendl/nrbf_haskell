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
        
        day     = (read (dates !! 2) :: Double)
        month   = (read (dates !! 1) :: Double)
        hour    = (read (times !! 0) :: Double)
        demand  = (read (parts !! 2) :: Double)

        row = (map (\f -> dp2 f 2) $
            (normalise_date [day, month, hour])) ++ [demand]
        in
            row

d :: [Int]
d = [1,1,2,3,1,1,2,2,3]
d2 :: [[Double]]
d2 = [[0, 0, 0, 0], [0,0,0,0], [0,0,1,0], [0,0,2,0], [0,0,1,0]]


d3 = [[3.0e-2,8.0e-2,0.0,32276.0],
    [3.0e-2,8.0e-2,0.0,32190.0],
    [3.0e-2,8.0e-2,0.0,32313.0],
    [3.0e-2,8.0e-2,0.0,32396.0],
    [3.0e-2,8.0e-2,0.0,32439.0],
    [3.0e-2,8.0e-2,0.0,32499.0],
    [3.0e-2,8.0e-2,0.0,32444.0],
    [3.0e-2,8.0e-2,0.0,32483.0],
    [3.0e-2,8.0e-2,0.0,32829.0],
    [3.0e-2,8.0e-2,0.0,32967.0],
    [3.0e-2,8.0e-2,0.0,33037.0],
    [3.0e-2,8.0e-2,0.0,33037.0],
    [3.0e-2,8.0e-2,4.0e-2,33027.0],
    [3.0e-2,8.0e-2,4.0e-2,32945.0],
    [3.0e-2,8.0e-2,4.0e-2,32988.0],
    [3.0e-2,8.0e-2,4.0e-2,32949.0],
    [3.0e-2,8.0e-2,4.0e-2,32804.0],
    [3.0e-2,8.0e-2,4.0e-2,32746.0],
    [3.0e-2,8.0e-2,4.0e-2,32629.0],
    [3.0e-2,8.0e-2,4.0e-2,32450.0],
    [3.0e-2,8.0e-2,4.0e-2,32294.0],
    [3.0e-2,8.0e-2,4.0e-2,32072.0],
    [3.0e-2,8.0e-2,4.0e-2,31862.0],
    [3.0e-2,8.0e-2,4.0e-2,31639.0],
    [3.0e-2,8.0e-2,8.0e-2,31419.0],
    [3.0e-2,8.0e-2,8.0e-2,31239.0],
    [3.0e-2,8.0e-2,8.0e-2,31153.0],
    [3.0e-2,8.0e-2,8.0e-2,31066.0],
    [3.0e-2,8.0e-2,8.0e-2,30824.0],
    [3.0e-2,8.0e-2,8.0e-2,30632.0]]

get_next_hour' :: [Double] -> [[Double]] -> [[Double]]
get_next_hour' h [] = [h]
get_next_hour' h t =
    case ((h !! 2) == ((head t) !! 2)) of
        True    -> get_next_hour' (head t) (tail t)
        False   -> [h] ++ (get_next_hour' (head t) (tail t))

get_next_hour :: [[Double]] -> [[Double]]
get_next_hour row = get_next_hour' (head row) (tail row)

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
        out_data_hourly = get_next_hour out_data
        out_data_s  = unlines $ map show (trace' out_data_hourly)

        in do
            -- print out first 10 hours of data
            putStrLn $ unlines $ map show $ take 10 out_data_hourly
            
            -- Write normalised training data to file
            --writeFile (file_name ++ ".out.csv") out_data_s