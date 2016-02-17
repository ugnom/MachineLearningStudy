module Week2.MoreCostFunction where

import System.Random
import Data.Vector (fromList, toList, Vector)
import Data.Matrix (colVector, transpose, multStd, (!))

type Parameter = Vector Double
type InputSet = Vector Double

data TrainingSet = TrainingSet { x :: InputSet, y :: Double }
  deriving (Show)

hyp :: Parameter -> InputSet -> Double
hyp theta x = (multStd (transpose (colVector theta)) (colVector x)) ! (1,1)

errorj :: Parameter -> [TrainingSet] -> Double
errorj theta tsets = 
  let h = hyp theta
  in (1 / (2 * (fromIntegral (length tsets)))) * (sum (map (\t -> ((h (x t)) - (y t))^2) tsets))

descentOne :: Double -> [TrainingSet] -> Parameter -> Parameter
descentOne alpha ts ps = fromList $ map (updateTheta) $ toList ps
  where 
    h = hyp ps
    m = fromIntegral . length $ ts
    updateTheta theta = theta - alpha * (sum (map (\t -> ((h (x t)) - (y t))) ts) / m)

gradientDescent :: Double -> Double -> [TrainingSet] -> Parameter -> Parameter
gradientDescent threshold alpha ts p = inGD p
  where
    inGD p =
      let up = d1 p  
      in if threshold > abs ((errorj p ts) - (errorj up ts))then p else inGD up 
    d1 = descentOne alpha ts

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