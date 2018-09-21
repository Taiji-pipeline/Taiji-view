{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE OverloadedStrings #-}

module Taiji.View.Commands.Rank
    ( plotRankParser
    , viewRanks
    ) where

import           Bio.Utils.Functions    (scale)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Arrow (first)
import           Data.Colour            (blend)
import qualified Data.Matrix            as M
import qualified Data.Vector            as V
import           Diagrams.Backend.Cairo (B)
import           Diagrams.Backend.Cairo (renderCairo)
import           Diagrams.Prelude       hiding (option, scale, value, normalize)
import           Graphics.SVGFonts      (textSVG)
import Data.Colour.Palette.BrewerSet
import           Options.Applicative
import           Statistics.Sample
import Statistics.Correlation (spearman)
import           Text.Printf

import           Taiji.View.Types
import           Taiji.View.Utils
import Taiji.View.Commands.Rank.Visualize

plotRankParser :: Parser Command
plotRankParser = fmap ViewRanks $ ViewRanksOpts
    <$> strOption
      ( long "expression" )
    <*> option auto
      ( long "cv"
     <> value 1
     <> help "TFs with coefficient of variance less than the specified value will be removed. (default: 1)"
      )
    <*> option auto
      ( long "min"
     <> value 1e-4
     <> help "lowerBound of TF rank." )
    <*> (optional . strOption) ( long "rowNamesFilter" )
    <*> (optional . strOption) ( long "output-values" )
