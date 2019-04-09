module Taiji.View.Types
    ( Options(..)
    , Command(..)
    ) where

newtype Options = Options Command

data Command = ViewRanks { rankFile :: FilePath
                         , output :: FilePath
                         , exprFile :: Maybe FilePath
                         , cv :: Double
                         , minRank :: Double
                         , rowNamesFilter :: Maybe FilePath
                         , outputValues :: Maybe FilePath
                         , rankRange :: Maybe (Double, Double)
                         , colGroup :: Maybe FilePath
                         }
             | ViewQC { qcFile :: FilePath
                      , outputDir :: FilePath
                      }