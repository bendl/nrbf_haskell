-- Gridwatch download preprocessor
module NRBF.Preprocess where

import NRBF.Network

import Data.List
import Data.List.Split
import System.Random (randomRIO)

-- Round f to n decimal places
dp2 f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

list_list_to_comma_str [] = ""
list_list_to_comma_str (row:rest) = 
    (list_to_comma_str row) ++ "\n" ++ (list_list_to_comma_str rest)

list_to_comma_str [] = ""
list_to_comma_str (x:[]) = show x
list_to_comma_str (x:xs) = 
    (show x) ++ "," ++ (list_to_comma_str xs)


shuffle x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)

-- [1..10] -> [(1,2), (3, 4), ...]
pairUp :: [a] -> [([a],a)]
pairUp [] = []
pairUp [a] = [] -- ignore outlying data
pairUp (tr:ts:tn) = ([tr], ts) : pairUp tn

pre_parsef :: String -> [Double]
pre_parsef str = do
    let l = lines str
        ns = filter (\n -> n /= "") l
        in map read ns :: [Double]

pre_xdata = pairUp . pre_parsef

normalise_date (d:m:h:[]) = [d/31, m/12, h/24]

pre_parse_line :: String -> [Double]
pre_parse_line line = do
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
        
        in row

get_next_hour' :: [Double] -> [[Double]] -> [[Double]]
get_next_hour' h [] =[h]
get_next_hour' h t =
    case (current_hour == next_hour) of
        True    -> 
            -- It's from the same hour, skip it
            get_next_hour' (head t) (tail t)
        False   -> 
            -- It's a new hour, append it to our output
            [h] ++ (get_next_hour' (head t) (tail t))
    where 
        current_hour = (h !! 2)
        next_hour    = ((head t) !! 2)

get_next_hour :: [[Double]] -> [[Double]]
get_next_hour row = get_next_hour' (head row) (tail row)

list_to_DSet :: [[Double]] -> DSet
list_to_DSet rows = 
    map (\r -> (take 3 r, r !! 3)) rows

-- Split the data into a 70/30 train/test split and store as tuple
split_70_30 :: [[Double]] -> ([[Double]], [[Double]])
split_70_30 d = do
    let len         = length d
        train_size  = round ((realToFrac len) * 0.7) :: Int
        in
            ((take train_size d), (drop train_size d))

write_70_30 :: String -> ([[Double]], [[Double]]) -> IO()
write_70_30 fn d = do
    let train_data_str  = list_list_to_comma_str $ fst d
        test_data_str   = list_list_to_comma_str $ snd d
        in do
            writeFile (fn ++ ".train.csv") train_data_str
            writeFile (fn ++ ".test.csv")  test_data_str
