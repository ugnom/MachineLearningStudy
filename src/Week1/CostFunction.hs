module Week1.CostFunction where

import System.Random

data TrainingSet = TrainingSet { x :: Double, y :: Double }
  deriving (Show)

data Parameter = Parameter { t0 :: Double, t1 :: Double }
  deriving (Show)

-- given 2 real number, returns a liniar function
hyp :: Parameter -> (Double -> Double)
hyp (Parameter t0 t1) = \x -> t0 + (t1 * x)

-- calcurate error of cost function using training sets.
errorf :: Parameter -> [TrainingSet] -> Double
errorf p ts = 
  let h = hyp p 
  in (sum (map (\t -> ((h (x t)) - (y t))^2) ts)) / (2 * (fromIntegral (length ts)))

-- iterative gradient descent until the difference from the previous gets less than th.
-- suitable alpha has to be found manually for appropriate descent of each time.
gradientDescent :: Double -> Double -> [TrainingSet] -> Parameter -> Parameter
gradientDescent th a ts p = inGD p
  where
    inGD p =
      let up = d1 p  
      in if th > abs ((errorf p ts) - (errorf up ts))then p else inGD up 
    d1 = descentOne a ts

-- one step of gradient descent.
-- using fixed derivative function to get a slope.
descentOne :: Double -> [TrainingSet] -> Parameter -> Parameter
descentOne  a ts p@(Parameter t0 t1) = 
  let h = hyp p
      m = fromIntegral . length $ ts 
      ut0 = t0 - a * (sum (map (\t -> ((h (x t)) - (y t))) ts) / m)
      ut1 = t1 - a * (sum (map (\t -> ((h (x t)) - (y t)) * (x t)) ts)  / m)
  in Parameter ut0 ut1


--creating sample data set by y = x with some randomness
composeDataSets :: Int -> IO [TrainingSet]
composeDataSets n = do
  gen0 <- newStdGen
  gen1 <- newStdGen
  let ls = zip (take n (randomRs (100.0::Double, 500.0::Double) gen0)) 
               (take n (randomRs (-0.1::Double, 0.1::Double) gen1))
  return $ map (\l -> TrainingSet (fst l) ((f (fst l)) * (1 + (snd l)))) ls


f :: Double -> Double
f x = x

