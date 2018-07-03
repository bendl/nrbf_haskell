module NRBF where

import Debug.Trace
import NRBF.Debug

import Control.Exception.Base

-- Euclidian distance
dist ps qs = sqrt $ sum $ map (^2) $ zipWith (-) ps qs

-- Guassian Radial Basis Function
grbf :: [Double] -> [Double] -> Double
grbf x_input x_existing =
    exp (- ((top) / (bottom)))
    where
        top = (dist x_input x_existing) ^^2
        bottom = 2 * (sigma ^^2)
        sigma = 1.0

weights_for_h 
    hidden_index input_len weights = do
        let stride = hidden_index * input_len
            stride_weights_start = trace' $ drop (trace' stride) weights
            in take input_len stride_weights_start


net :: [Double] -> [[Double]]  -> Double
net inputs weights = 
    assert (length inputs == length ( weights !! 0))
        sum hidden_outs
    where 
        hidden_outs = map (\w -> grbf inputs w) w 
        w           = trace'str "Weights: " weights

train :: [Double] -> [[Double]] -> Double -> Double
train inputs weights target = do
    let net_out = net inputs weights
        in 0
