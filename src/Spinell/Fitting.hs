{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


module Spinell.Fitting
    (fit, 
     unscaledFit,
     mvarFit,
     mvarUnscaledFit,
     defFitOpt,
     FitOptions(..),
     Jacobianable(..),
     Model,
     UnboxedModel,
     getParams,
     getUnscaledParams) where


import Numeric.GSL.Fitting
import Numeric.LinearAlgebra
import Numeric.AD
import Numeric.AD.Internal.Reverse
import Data.Reflection


type FitData = [([Double],([Double],Double))]
type FitRes = ([(Double, Double)], Matrix Double)
type UnscaledFitRes = ([Double], Matrix Double)

--
type Model = (forall a . Floating a => [a]->[a]->[a])

type UnboxedModel = (forall a . Floating a => [a]->a->a)
 
box :: UnboxedModel -> Model
box unmodel = (\p -> \x -> return (unmodel p (x !! 0)))

--
type FullJacob = ([Double] -> [Double] -> [[Double]])

data Jacobianable = AutoJacob | ManualJacob FullJacob

sanitize :: Jacobianable -> Model -> FullJacob
sanitize AutoJacob model = mkJac model
sanitize (ManualJacob jac) model = jac

--

data FitOptions = FitOptions { 
        jacob :: Jacobianable,
        iter :: Int, 
        absTol :: Double, 
        relTol :: Double
    } 

defFitOpt = FitOptions { jacob = AutoJacob,iter = 1000,absTol = 1E-20,relTol = 1E-20 }

--

fit :: [Double]
     -> [Double]
     -> [Double]
     -> UnboxedModel
     -> [Double]
     -> FitOptions
     -> FitRes

fit xs ys sigma rawmodel guess fitparams
    | xeqy && ( seqx || seqone) = fitModelScaled setAbsTol setResTol setIter (box rawmodel, sanitize jacobian (box rawmodel)) fitdata guess
    | otherwise = error "Input data has the wrong size"
    where 
    lx = length xs
    ly = length ys
    ls = length sigma
    xeqy = lx == ly
    seqx = ls == lx
    seqone = ls == 1

    jacobian = jacob fitparams --we extract the "jacobian" parameter from the record

    setIter = iter fitparams
    setAbsTol = absTol fitparams
    setResTol = relTol fitparams

    fitdata = formatData xs ys sigma


formatData :: [Double] -> [Double] -> [Double] -> FitData
formatData xs ys sigma
    |  length sigma == length xs = zip (return <$> xs) $ zip (return <$> ys) sigma
    |  length sigma == 1 = zip (return <$> xs) $ zip (return <$> ys) (repeat (head sigma))


unscaledFit :: [Double]
     -> [Double]
     -> UnboxedModel
     -> [Double]
     -> FitOptions
     -> UnscaledFitRes

unscaledFit xs ys rawmodel guess fitparams
    | xeqy = fitModel setAbsTol setResTol setIter (box rawmodel, sanitize jacobian (box rawmodel)) fitdata guess
    | otherwise = error "Input data has the wrong size"
    where 
    lx = length xs
    ly = length ys
    xeqy = lx == ly

    jacobian = jacob fitparams

    setIter = iter fitparams
    setAbsTol = absTol fitparams
    setResTol = relTol fitparams

    fitdata = zip (return <$> xs) (return <$> ys) 

mvarFit :: [[Double]]
     -> [[Double]]
     -> [Double]
     -> Model
     -> [Double]
     -> FitOptions
     -> FitRes

mvarFit xs ys sigma model guess fitparams
    | xeqy && ( seqx || seqone) = fitModelScaled setAbsTol setResTol setIter (model, sanitize jacobian model) fitdata guess
    | otherwise = error "Input data has the wrong size"
    where 
    lx = length xs
    ly = length ys
    ls = length sigma
    xeqy = lx == ly
    seqx = ls == lx
    seqone = ls == 1

    jacobian = jacob fitparams

    setIter = iter fitparams
    setAbsTol = absTol fitparams
    setResTol = relTol fitparams

    fitdata = formatDataBoxed xs ys sigma


formatDataBoxed :: [[Double]] -> [[Double]] -> [Double] -> FitData
formatDataBoxed xs ys sigma
    |  length sigma == length xs = zip xs $ zip ys sigma
    |  length sigma == 1 = zip xs $ zip ys (repeat (head sigma))

mvarUnscaledFit :: [[Double]]
     -> [[Double]]
     -> Model
     -> [Double]
     -> FitOptions
     -> UnscaledFitRes

mvarUnscaledFit xs ys model guess fitparams
    | xeqy = fitModel setAbsTol setResTol setIter (model, sanitize jacobian model) fitdata guess
    | otherwise = error "Input data has the wrong size"
    where 
    lx = length xs
    ly = length ys
    xeqy = lx == ly

    jacobian = jacob fitparams

    setIter = iter fitparams
    setAbsTol = absTol fitparams
    setResTol = relTol fitparams

    fitdata = zip xs ys

mkJac :: (Num a) => (forall s. Reifies s Tape => [Reverse s a] -> [Reverse s a] -> [Reverse s a])
       -> [a]
       -> [a]
       -> [[a]]

mkJac f p x = jacobian ((flip f) (fmap auto x)) p

getParams :: FitRes -> [Double]
getParams res = fst . unzip . fst $ res

getUnscaledParams :: UnscaledFitRes -> [Double]
getUnscaledParams res = fst res
