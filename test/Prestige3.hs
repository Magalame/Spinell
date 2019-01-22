{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, GADTs,
             OverloadedStrings, PatternSynonyms, QuasiQuotes,
             ScopedTypeVariables, TemplateHaskell, TypeApplications,
             TypeOperators, ViewPatterns #-}

module TestDat 
( sampdat,
mkframe
) where 

import qualified Data.Foldable as F
import Data.Vinyl
import Data.Vinyl.Functor (Identity(..), Const(..))
import Lens.Micro
import Lens.Micro.Extras
import Frames
import Frames.CSV 

tableTypes "Row" "Prestige.csv"

loadRows :: IO (Frame Row)
loadRows = inCoreAoS (readTable "Prestige.csv")

sampdat = do 
    frame <- loadRows
    let inclist =  F.toList $ (view income <$> frame)
        b = "a"
    return inclist


mkframe = do 
        frame <- loadRows
        return frame



-- [[../plotPrestige.png]]
