-- Gridwatch download preprocessor
module NRBF.Preprocess where

--import nrbf

import Data.List
import Data.List.Split

import Debug.Trace
trace' arg = trace (show arg) arg

-- Train Test split
newtype Tdata = Tdata [Double]
newtype Tpair = Tpair (Double, Double)
newtype Tts = Tts (Tdata, Tdata)

-- Test data
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

train10x = "-0.250000\n\n-0.048348\n\n\n0.000000\n\n0.000000\n\n\n0.5\n\n0.138"

-- [1..10] -> [(1,2), (3, 4), ...]
pairUp :: [a] -> [(a,a)]
pairUp [] = []
pairUp [a] = [] -- ignore outlying data
pairUp (tr:ts:tn) = (tr, ts) : pairUp tn

parsef :: String -> [Double]
parsef str = do
    let l = lines str
        ns = filter (\n -> n /= "") l
        in map read ns :: [Double]

xdata = pairUp . parsef

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

-- Split the data into a 70/30 train/test split and store as tuple
split_70_30 :: [[Double]] -> ([[Double]], [[Double]])
split_70_30 d = do
    let len         = length d
        train_size  = round ((realToFrac len) * 0.7) :: Int
        in
            ((take train_size d), (drop train_size d))

write_70_30 :: String -> ([[Double]], [[Double]]) -> IO()
write_70_30 fn d = do
    let train_data_str  = unlines $ map show $ fst d
        test_data_str   = unlines $ map show $ snd d
        in do
            writeFile (fn ++ ".train.csv") train_data_str
            writeFile (fn ++ ".test.csv")  test_data_str

-- Euclidian distance
dist ps qs = sqrt $ sum $ map (^2) $ zipWith (-) ps qs

grbf :: [Double] -> [Double] -> Double
grbf x xj =
    exp (- ((top) / (bottom)))
    where
        top = (dist x xj) ^^2
        bottom = 2 * (sigma ^^2)
        sigma = 5.0

nrbf :: Double
nrbf = do
    let d = 3
        in 3.0