{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module Lib
    ( someFunc
    ) where


import Numeric.GSL.Fitting
import Numeric.LinearAlgebra
import Numeric.AD
import Numeric.AD.Internal.Reverse
import Data.Reflection

someFunc :: IO ()
someFunc = putStrLn "someFunc"


type FitRes = ([(Double, Double)], Matrix Double)
type FitData = [([Double],([Double],Double))]

-- type ManualJacob = ([Double] -> [Double] -> [[Double]]) 

data JacobType = AutoJacob | ManualJacob ([Double] -> [Double] -> [[Double]])

data FitParams = FitParams { jacob :: JacobType
                           , guess :: [Double]
                           , iter :: Int 
                           , absTol :: Double  
                           , relTol :: Double                                 
                           }  

defaultParams = FitParams { jacob = AutoJacob,
                            guess = repeat 1,
                            iter = 1000,
                            absTol = 1E-20,
                            relTol = 1E-20 }

     -- ([Double] -> [Double] -> [Double]) model
     -- ([Double] -> [Double] -> [[Double]]) model der
-- Only Doubles since hmatrix requires it
fit :: [Double]
     -> [Double]
     -> [Double]
     -> (forall a . Floating a => [a]->[a]->[a])
     -> JacobType
     -> [Double]->  Maybe FitRes



fit xs ys sigma model jacobian guess
    | xeqy && ( seqx || seqone)  = Just $ fitModelScaled 1E-20 1E-20 500 (model, sanitizedJacob jacobian) fitdata guess
    | otherwise  = Nothing
    where 
    lx = length xs
    ly = length ys
    ls = length sigma
    xeqy = lx == ly
    seqx = ls == lx
    seqone = ls == 1
    fitdata = unsafeFormatData xs ys sigma
    sanitizedJacob jacob = case jacob of AutoJacob -> mkJac model
                                         ManualJacob f -> f


{-
fit :: [Double]
     -> [Double]
     -> [Double]
     -> (forall a . Floating a => [a]->[a]->[a])
     -> ([Double] -> [Double] -> [[Double]])
     -> [Double]->  Maybe FitRes



fit xs ys sigma model jacobian guess
    | xeqy && ( seqx || seqone)  = Just $ fitModelScaled 1E-20 1E-20 500 (model, mkJac model) fitdata guess
    | otherwise  = Nothing
    where 
    lx = length xs
    ly = length ys
    ls = length sigma
    xeqy = lx == ly
    seqx = ls == lx
    seqone = ls == 1
    fitdata = unsafeFormatData xs ys sigma
-}

{-
fit :: [Double]
     -> [Double]
     -> [Double]
     -> ([Double] -> [Double] -> [Double])
     -> ([Double] -> [Double] -> [[Double]])
     -> [Double]->  Maybe FitRes



fit xs ys sigma model jacobian guess
    | xeqy && ( seqx || seqone)  = Just $ fitModelScaled 1E-20 1E-20 500 (model, jacobian) fitdata guess
    | otherwise  = Nothing
    where 
    lx = length xs
    ly = length ys
    ls = length sigma
    xeqy = lx == ly
    seqx = ls == lx
    seqone = ls == 1
    fitdata = unsafeFormatData xs ys sigma -}


unsafeFormatData :: [Double] -> [Double] -> [Double] -> FitData
unsafeFormatData xs ys sigma
    |  length sigma == length xs = zip (return <$> xs) $ zip (return <$> ys) sigma
    |  length sigma == 1 = zip (return <$> xs) $ zip (return <$> ys) (repeat (head sigma))



--jac :: (Num a, Traversable f, Functor g) =>
  --     (forall s. Reifies s Tape => f (Reverse s a) -> [Reverse s a] -> g (Reverse s a))
    --   -> f a
      -- -> g (f a)

jac2 :: (Num a, Traversable f1, Traversable f2, Functor g) =>
       (forall s. Reifies s Tape => f1 (Reverse s a) -> f2 (Reverse s a) -> g (Reverse s a))
       -> f1 a
       -> f2 a
       
       -> g (f1 a)

jac2 f p x = jacobian ((flip f) (fmap auto x)) p

jac3 :: (Floating a, Traversable f1, Traversable f2, Functor g) =>
       (forall s. Reifies s Tape => f1 (Reverse s a) -> f2 (Reverse s a) -> g (Reverse s a))
       -> f1 a
       -> f2 a
       
       -> g (f1 a)
 
jac3 f p x = jacobian ((flip f) (fmap auto x)) p


mkJac :: (Num a) => (forall s. Reifies s Tape => [Reverse s a] -> [Reverse s a] -> [Reverse s a])
       -> [a]
       -> [a]
       
       -> [[a]]

mkJac f p x = jacobian ((flip f) (fmap auto x)) p

-- to fit data
expModel [a,lambda,b] [t] = [a * exp (-lambda * t)+b]

expModelDer [a,lambda,b] [t] = [[exp (-lambda * t), -t * a * exp(-lambda*t) , 1]]

-- to generate data
expModel2 [a,lambda,b] t = a * exp (-lambda * t)+b

expModelDer2 [a,lambda,b] t = [[exp (-lambda * t), -t * a * exp(-lambda*t) , 1]]
-- to go from unbox to box:
-- a = (\p -> \x -> return (expModel2 p (x !! 0)))::Floating a => [a] -> [a] -> [a]

xs = [a | a <- [0..100]]
sigma = [0.1]
ys = (map (expModel2 [5,0.1,1]) xs)




