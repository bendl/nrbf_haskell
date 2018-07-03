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

-- https://stackoverflow.com/questions/1496980/finding-index-of-element-in-a-list-in-haskell
maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

-- Applies the delta learning rule
-- (0.8 * old) + (0.2 * new)
delta_rule :: DInput -> DInput -> DInput
delta_rule xold xnew = 
    zipWith (+) (map (* 0.8) xold) (map (* 0.2) xnew)

-- Update hidden layer, depending on training result
--  If 
update :: DRow -> DWeights -> DWeights
update td [] = trace "No weights... Setting initial to input..." [fst td]
update td weights = do
    -- 1. Find most active hidden node for (fst DRow)
    -- 2. Apply (0.8 * active hidden node weights) + (0.2 * fst DRow)
    --trace'str "Most active hnode: " most_active_index
    updated_weights

    where   td_input = fst td
            td_expec = snd td
            most_active_index = maxIndex $ map (\i -> grbf td_input i) weights
            updated_weights = delta_rule (head weights) td_input

td = [([5.0], 1.5)]
-- type DSet     = [(DInput, DOutput)]
-- type DSet     = [([0], 0)]
-- Training accepts an input data set, an initial set of weights,
-- and returns a trained set of weights
train' :: DSet -> DWeights -> DWeights
-- Dataset, but no weights
train' dataset [] = train' dataset $ update (head dataset) []
-- Empty dataset with weights
train' [] weights = trace'str "Empty dataset! Returning..." weights
train' [] [] = error "DSet and DWeights cannot be empty!"
train' dataset weights = do
    trace ("Train Err: " ++ (show train_err)) weights

    -- TODO: Change to test RMS error, not train error
    case (train_err <= 0.1)  of
        True    ->
            -- Good prediction, don't change hidden layer
            weights
        False   ->
            -- Bad prediction, update hidden layer
            train' (trace'str "\n\nNext dataset: " tdn) (update tdi weights)

    where
        tdi         = trace'str "tdi: " $ head dataset
        td_input    = fst tdi
        td_expec    = snd tdi
        tdn         = drop 1 dataset
        nn_out      = net td_input weights
        train_err   = (trace'str "NN_expec: " td_expec) 
                        - (trace'str "NN_output: " nn_out)