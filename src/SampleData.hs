{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, GADTs,
             OverloadedStrings, PatternSynonyms, QuasiQuotes,
             ScopedTypeVariables, TemplateHaskell, TypeApplications,
             TypeOperators, ViewPatterns #-}

module SampleData 
( sampdat,expModel,expModelDer,expModel2,expModelDer2,xs,ys,sigma
) where 

import qualified Data.Foldable as F
import Data.Vinyl
import Data.Vinyl.Functor (Identity(..), Const(..))
import Lens.Micro
import Lens.Micro.Extras
import Frames
import Frames.CSV 

tableTypes "Row" "/home/nouey/Projects/Spinell/test/Prestige.csv"

loadRows :: IO (Frame Row)
loadRows = inCoreAoS (readTable "/home/nouey/Projects/Spinell/test/Prestige.csv")

sampdat = do 
    frame <- loadRows
    let inclist =  F.toList $ (view income <$> frame)
        b = "a"
    return inclist


-- to fit data
expModel [a,lambda,b] [t] = [a * exp (-lambda * t)+b]

expModelDer [a,lambda,b] [t] = [[exp (-lambda * t), -t * a * exp(-lambda*t) , 1]]

-- to generate data
expModel2 [a,lambda,b] t = a * exp (-lambda * t)+b

expModelDer2 [a,lambda,b] t = [[exp (-lambda * t), -t * a * exp(-lambda*t) , 1]]

xs = [a | a <- [0..100]]
sigma = [0.1]
ys = (map (expModel2 [5,0.1,1]) xs)
