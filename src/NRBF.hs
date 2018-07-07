module NRBF where

import Debug.Trace
import NRBF.Debug

import Data.List (genericLength)

import Control.Exception.Base

type DInput     = [Double]
type DOutput    = Double
type DHidden    = [DInput]
type DWeights   = [Double]
type DNet       = (DHidden, DWeights)
type DRow       = (DInput, DOutput)
type DSet       = [DRow]

-- Euclidian distance
dist ps qs = sqrt $ sum $ map (^2) $ zipWith (-) ps qs

-- Guassian Radial Basis Function
grbf :: DInput -> DInput -> Double
grbf x_input x_existing =
    exp (- ((top) / (bottom)))
    where
        top = (dist x_input x_existing) ^^2
        bottom = 2 * (sigma ^^2)
        sigma = 0.05

weights_for_h 
    hidden_index input_len weights = do
        let stride = hidden_index * input_len
            stride_weights_start = trace' $ drop (trace' stride) weights
            in take input_len stride_weights_start


net :: DInput -> DNet -> Double
net inputs network = 
    assert (length inputs == length (hidden_nodes !! 0))
        (sum hidden_outs_weighted) / (sum hidden_outs)
    where 
        hidden_nodes            = fst network
        network_weights         = snd network
        hidden_outs             = map (\hw -> grbf inputs hw) hidden_nodes
        hidden_outs_weighted    = zipWith (*) hidden_outs network_weights

-- https://stackoverflow.com/questions/1496980/finding-index-of-element-in-a-list-in-haskell
maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

-- Applies the delta learning rule
-- (0.8 * old) + (0.2 * new)
delta_rule :: DInput -> DInput -> DInput
delta_rule xold xnew = 
    zipWith (+) (map (* 0.8) xold) (map (* 0.2) xnew)

    --type DOutput  = Double
    --type DInput   = [Double]
    --type DWeights = [DInput]
    --type DRow     = (DInput, DOutput)
    --type DSet     = [DRow]
    
--                [ [] ]
hidden_replace :: DHidden -> Int -> DInput -> DHidden
hidden_replace all_hidden idx new_hidden = 
    (take idx all_hidden) ++ new_hidden : (drop (idx + 1) all_hidden)

weight_replace :: DWeights -> Int -> Double -> DWeights
weight_replace all_weights idx new_weight = 
    (take idx all_weights) ++ new_weight : (drop (idx + 1) all_weights)


-- Update hidden layer, depending on training result
--  If 
update :: DRow -> Double -> DNet -> DNet
update td error ([], _) = trace "No weights... Setting initial to input..." ([fst td], [snd td])
update td error network = do
    -- 1. Find most active hidden node for (fst DRow)
    -- 2. Apply (0.8 * active hidden node weights) + (0.2 * fst DRow)
    --trace'str "Most active hnode: " most_active_index
    trace'str "Final Network: " (new_hidden_nodes, new_hidden_weights)

    where   
            hidden_nodes        = fst network
            net_weights         = snd network
            td_input            = fst td
            td_expec            = snd td
            node_activity       = map (\i -> grbf td_input i) hidden_nodes
            most_active_index   = maxIndex node_activity
            most_active_node_hf = node_activity !! most_active_index
            most_active_node    = hidden_nodes  !! most_active_index
            most_active_weight  = net_weights   !! most_active_index

            updated_hidden      = delta_rule most_active_node td_input
            updated_weight      = most_active_weight + (0.2 * error)

            new_hidden_nodes    = 
                case ((trace' most_active_node_hf) >= 0.9) of
                    True    -> hidden_replace hidden_nodes most_active_index updated_hidden
                    False   -> hidden_nodes ++ [td_input]
            new_hidden_weights  = 
                case ((trace' most_active_node_hf) >= 0.9) of
                    True    -> weight_replace net_weights most_active_index updated_weight
                    False   -> net_weights ++ [error]
            

td = [([5.0], 1.5)]
td2 = [([5.0], 1.5), ([3.0], 0.8), ([2.8], 0.6)]

rms_ll :: [Double] -> [Double] -> Double
rms_ll pred actual = 
    rootMeanSquare $ zipWith (-) pred actual

rootMeanSquare :: [Double] -> Double
rootMeanSquare = sqrt . (((/) . foldr ((+) . (^ 2)) 0) <*> genericLength)

-- type DSet     = [(DInput, DOutput)]
-- type DSet     = [([0], 0)]
-- Training accepts an input data set, an initial set of weights,
-- and returns a trained set of weights
train' :: DSet -> DNet -> [Double] -> (DNet, [Double])
train' [] ([], []) _ = error "DSet and DWeights cannot be empty!"
-- Dataset, but no weights
train' dataset ([], _) rms = train' dataset ([(fst $ head dataset)], [snd $ head dataset]) rms
-- Empty dataset with weights
train' [] network rms = (network, rms)
-- Dataset and weights present
train' dataset network rms = do
    case (perf <= 0.1)  of
        True    ->
            -- Good prediction, don't change hidden layer
            (network, new_rms)
        False   ->
            -- Bad prediction, update hidden layer
            train' tdn updated_net new_rms

    where
        tdi         = head dataset
        td_input    = fst tdi
        td_expec    = snd tdi
        tdn         = drop 1 dataset
        current_net = network
        nn_out      = net td_input current_net
        train_err   = td_expec - nn_out
        new_rms     = rms ++ [abs train_err]
        perf        = rootMeanSquare new_rms
        updated_net = update tdi train_err current_net

-- Input Dataset, iterations, Output RMS error
fit :: DSet -> Int -> (DNet, [Double])
fit dataset 0 = (([], []), [-1.0])
fit dataset 1 = train' dataset ([], []) [1.0]
fit dataset iterations = 
    train' dataset network initial_rms
    where
        initial_rms = [1.0]
        network = fst $ fit dataset (iterations -1)
