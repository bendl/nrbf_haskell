module NRBF where

import Debug.Trace
import NRBF.Debug

import Control.Exception.Base

type DInput   = [Double]
type DOutput  = Double
type DWeights = [DInput]
type DRow     = (DInput, DOutput)
type DSet     = [DRow]

-- Euclidian distance
dist ps qs = sqrt $ sum $ map (^2) $ zipWith (-) ps qs

-- Guassian Radial Basis Function
grbf :: DInput -> DInput -> Double
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


net :: DInput -> DWeights  -> Double
net inputs weights = 
    assert (length inputs == length ( weights !! 0))
        sum hidden_outs
    where 
        hidden_outs = map (\w -> grbf inputs w) w 
        w           = trace'str "Weights: " weights

-- Update hidden layer, depending on training result
--  If 
update :: DRow -> DWeights -> DWeights
update td weights = do
    weights

td = [([5.0], 1.5)]
-- type DSet     = [(DInput, DOutput)]
-- type DSet     = [([0], 0)]
-- Training accepts an input data set, an initial set of weights,
-- and returns a trained set of weights
train' :: DSet -> DWeights -> DWeights
train' [] weights = trace "Empty dataset! Returning..." weights
train' dataset weights = do
    trace ("Train Err: " ++ (show train_err)) weights
    case (train_err <= 0.1)  of
        True    ->
            -- Good prediction, don't change hidden layer
            weights
        False   ->
            -- Bad prediction, update hidden layer
            train' (trace'str "Next dataset: " tdn) (update tdi weights)

    where
        tdi         = head (trace' dataset)
        td_input    = fst tdi
        td_expec    = snd tdi
        tdn         = drop 1 dataset
        nn_out      = net td_input weights
        train_err   = td_expec - nn_out