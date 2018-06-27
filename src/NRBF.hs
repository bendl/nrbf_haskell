module NRBF where

-- Euclidian distance
dist ps qs = sqrt $ sum $ map (^2) $ zipWith (-) ps qs

-- Guassian Radial Basis Function
grbf :: [Double] -> [Double] -> Double
grbf x xj =
    exp (- ((top) / (bottom)))
    where
        top = (dist x xj) ^^2
        bottom = 2 * (sigma ^^2)
        sigma = 5.0

net :: [Double] -> [Double] -> Double
net is hs = do
    