module Test_Fit where

import System.Environment
import Control.Monad (when)

import NRBF.Preprocess
import NRBF.Network

test_fit3d :: Int -> IO()
test_fit3d iterations = do
    
    v
    
    let 
        file_name       = "data//gridwatch.2013-2014.csv"

        fn_out          = file_name ++ ".out.csv"
        fn_out_train    = fn_out ++ ".train.csv"
        fn_out_test     = fn_out ++ ".test.csv"

        file_lines      = lines filec
        header          = take 1 file_lines
        file_data       = drop 1 file_lines
        train_data      = file_data
        out_data        = map pre_parse_line train_data
        out_data_hourly = get_next_hour out_data

        ttd             = split_70_30 out_data_hourly

        gridwatch_train = list_to_DSet $ fst ttd
        gridwatch_test  = list_to_DSet $ snd ttd

        trained_result  = fit gridwatch_train iterations
        trained_net     = fst trained_result
        trained_neth    = fst trained_net
        trained_netw    = snd trained_net
        
        r               = [0.0, 0.05 .. 1.0]
        out_input       = [[x,y,z] | x <- r, y <- r, z <- r]

        in do
            --write_70_30 file_name ttd
            --putStrLn "Done"

            --putStrLn "Grid watch train: "
            --putStrLn ((unlines $ map show $ gridwatch_train) ++ " = ")
            
            writeFile "grbf_3d_input.csv" $ unlines $ map show [snd exp | exp <- gridwatch_test]

            putStrLn ("Number of hidden nodes: " ++ show (length trained_neth))

            writeFile "grbf_3d.csv" $ unlines $ map show $ map (\n -> predict n trained_net) out_input
            putStrLn "Done"

test_fit1d :: Int -> IO()
test_fit1d iterations = do
    train10x_s  <- readFile "data//TRAIN10X.DAT"
    
    let
        traind10        = pre_xdata train10x_s
        train_result    = fit traind10 iterations
        trained_net     = fst train_result
        trained_neth    = fst trained_net
        trained_netw    = snd trained_net

        in do
            putStrLn ("Hidden Nodes: " ++ (show (length (snd trained_net))))
            putStrLn $ unlines $ map show $ trained_neth

            putStrLn "Hidden Weights: "
            putStrLn $ unlines $ map show $ trained_netw

            putStrLn $ "RMSE: " ++ (show $ rootMeanSquare $ snd trained_net)

            writeFile "grbf.csv" $ unlines $ map show $ map (\n -> predict [n] trained_net) [-1.0, -0.9 .. 5.0]

main :: IO()
main = do
    putStrLn "Foo"