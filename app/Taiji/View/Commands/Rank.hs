{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE OverloadedStrings #-}

module Taiji.View.Commands.Rank
    ( plotRankParser
    , viewRanks
    ) where

import qualified Data.Text as T
import           Options.Applicative

import           Taiji.View.Types
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
    <*> (optional . option (maybeReader f)) ( long "rank-range" )
    <*> (optional . strOption) ( long "groups" )
  where
    f x = let [a,b] = T.splitOn "," $ T.pack x
          in Just (read $ T.unpack a, read $ T.unpack b)
