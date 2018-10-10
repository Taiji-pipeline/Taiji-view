module Taiji.View.Types
    ( Options(..)
    , Command(..)
    , ShowRegulTableOpts(..)
    , ViewRanksOpts(..)
    ) where

data Options = Options FilePath FilePath Command

data Command = ShowRegulTable ShowRegulTableOpts
             | ShowNetwork
             | ViewRanks ViewRanksOpts

data ShowRegulTableOpts = ShowRegulTableOpts
    { expression_fl :: FilePath
    , logScale :: Bool
    , percent :: Double
    , binarize :: Maybe Double }

data ViewRanksOpts = ViewRanksOpts
    { exprFile :: FilePath
    , cv :: Double
    , minRank :: Double
    , rowNamesFilter :: Maybe FilePath
    , outputValues :: Maybe FilePath
    , rankRange :: Maybe (Double, Double)
    }