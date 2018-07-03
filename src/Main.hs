module Main where

import System.Environment
import Control.Monad (when)

import NRBF.Preprocess
import NRBF


-- :main gridwatch.2013-2014.csv
main :: IO()
main = do
    args  <- getArgs
    when ((length args) == 0) $ 
        error "Missing input file"

    filec <- readFile $ args !! 0

    train10x_s  <- readFile "data//TRAIN10X.DAT"
    test10x_s   <- readFile "data//TEST10X.DAT"
    
    let 
        file_name       = args !! 0

        fn_out          = file_name ++ ".out.csv"
        fn_out_train    = fn_out ++ ".train.csv"
        fn_out_test     = fn_out ++ ".test.csv"

        file_lines      = lines filec
        header          = take 1 file_lines
        file_data       = drop 1 file_lines
        test_data       = file_data
        out_data        = map pre_parse_line test_data
        out_data_hourly = get_next_hour out_data

        ttd             = split_70_30 out_data_hourly

        out_data_s      = unlines $ map show out_data_hourly

        train10x_d      = pre_xdata train10x_s
        test10x_d       = pre_xdata test10x_s

        in do

            -- print out first 10 hours of data
            --putStrLn $ unlines $ map show $ take 10 out_data_hourly

            -- Write normalised training data to file
            putStrLn "Writing normalised data to .out.csv"
            --writeFile fn_out out_data_s

            -- Write train and test data split to file
            putStrLn "Writing 70/30 split to (.train/.test).csv"
            --write_70_30 fn_out ttd

            putStrLn ("10X_tr:\n" ++ (unlines $ map show train10x_d))
            putStrLn ("10X_ts:\n" ++ (unlines $ map show test10x_d))