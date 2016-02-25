module Week3.LogisticRegression where

import Data.Matrix

-- called Sigmoid Function or Logistic function.
-- round any values to 0 to 1 (but in what way?)
sigmoid :: Double -> Double
sigmoid z = 1 / (1 + exp(-z))

-- theta & x both should be (n x 1) matrix
hypoTheta :: Matrix Double -> Matrix Double -> Double
hypoTheta theta x = sigmoid $ (multStd (transpose theta) x) ! (1,1)

-- cost functions to be either 1 or 0)
cost1 h x = (-1) * (log (h x))
cost0 h x = (-1) * (log (1 - (h x)))