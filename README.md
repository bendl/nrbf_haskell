# NRBF Neural Network using Haskell

![NRBF Network Structure](https://github.com/bendl/nrbf_haskell/blob/master/doc/nrbf.png)

## Install
```sh
cabal install --enable-tests
cabal test
```

## Usage 
Import the NRBF data Preprocessor and Network:
```haskell
import NRBF.Preprocess -- Optional, But NRBF.Network only accepts training data in DSet form
import NRBF.Network
```

Training data must be of type `DSet`
```haskell
-- Network data types
type DInput     = [Double]              -- 1D input vector
type DOutput    = Double                -- Limited to 1D output for now
type DHidden    = [DInput]              -- Hidden nodes
type DWeights   = [Double]              -- Output weights
type DNet       = (DHidden, DWeights)   -- Network container (Hidden nodes, output weights)
type DRow       = (DInput, DOutput)     -- Training item (Input, Expected output)
type DSet       = [DRow]                -- List of training items

-- Dataset -> Iterations -> (Dnet, [RMS errors])
-- Use this to fit a function to your training data
fit :: DSet -> Int -> (DNet, [Double])
```

## Example Program

Currently, only formats like `test/data/TRAIN10X.DAT` and `test/data/gridwatch.2013-2014.csv` are supported.
The NRBF network supports N-dimensional inputs but is limited to 1 output (for now).

Example 1D input (TRAIN10X.DAT, function to approximate input/4):
```haskell
test_fit_1d iterations = do
    train10x_s <- readFile "test//data//TRAIN10X.DAT"
    
    let
        traind10        = pre_xdata train10x_s
        train_result    = fit traind10 iterations -- Fit the network to the training data
        trained_net     = fst train_result

        in writeFile "output.csv" $ unlines $ map show $ map (\n -> predict [n] trained_net) [-1.0, -0.9 .. 5.0]
```

Example 3D input (gridwatch.2013-2014.csv):
```haskell
test_fit3d iterations = do
    filec <- readFile $ "data//gridwatch.2013-2014.csv"
    
    let 
        out_data_hourly = preprocess_gridwatch_hour filec
        ttd             = split_70_30 out_data_hourly

        gridwatch_train = list_to_DSet $ fst ttd

        trained_result  = fit gridwatch_train iterations -- Fit the network to the training data
        trained_net     = fst trained_result
        trained_neth    = fst trained_net -- Hidden nodes
        trained_netw    = snd trained_net -- Weights
        
        -- 3D input test vector
        r               = [0.0, 0.05 .. 1.0]
        out_input       = [[x,y,z] | x <- r, y <- r, z <- r]

        in do
            putStrLn ("Number of hidden nodes: " ++ show (length trained_neth))
            -- Show predicted values of the test input vector
            writeFile "grbf_3d.csv" $ unlines $ map show $ map (\n -> predict n trained_net) out_input
            putStrLn "Done"
```

## Example Outputs
TODO