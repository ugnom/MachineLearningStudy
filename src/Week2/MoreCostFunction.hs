module Week2.MoreCostFunction where

import System.Random
import Data.Vector (fromList, toList, Vector)
import Data.Matrix (colVector, transpose, multStd, (!))

type Parameter = Vector Double
type InputSet = Vector Double

data TrainingSet = TrainingSet { x :: InputSet, y :: Double }
  deriving (Show)

--Hypotethis of the learning, applying parameter after leaned
hyp :: Parameter -> InputSet -> Double
hyp theta x = (multStd (transpose (colVector theta)) (colVector x)) ! (1,1)

--error function, how much error rate it gets with a given parameter
errorj :: Parameter -> [TrainingSet] -> Double
errorj theta tsets = 
  let h = hyp theta
  in (1 / (2 * (fromIntegral (length tsets)))) * (sum (map (\t -> ((h (x t)) - (y t))^2) tsets))

--Gradient Decent with multi-parameter/input values. 
gradientDescent :: Double -> Double -> [TrainingSet] -> Parameter -> Parameter
gradientDescent threshold alpha ts p = inGD p
  where
    inGD p =
      let up = d1 p  
      in if threshold > abs ((errorj p ts) - (errorj up ts))then p else inGD up 
    d1 = descentOne alpha ts

descentOne :: Double -> [TrainingSet] -> Parameter -> Parameter
descentOne alpha ts ps = fromList $ map (updateTheta) $  zip [0..] (toList ps)
  where 
    h = hyp ps
    m = fromIntegral . length $ ts
    updateTheta (n, theta) = 
      theta - alpha * (sum (map (\t -> ((h (x t)) - (y t)) * ((toList (x t)) !! n)) ts) / m)


--scale a input feature to roughly -1 to 1 (which makes gradient descent faster)
--the way the feature has been scaled should be kept some where to apply it later to test set?
-- (value - ave) / (max - min) <-- we can apply S.D. concept but don't have to 
scaleFeature :: [Double] -> [Double]
scaleFeature = undefined

{-
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

-}