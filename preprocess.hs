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
            writeFile (file_name ++ ".out.csv") out_data_s