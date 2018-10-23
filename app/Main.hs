{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Main where

import           Data.Monoid         ((<>))
import           Options.Applicative

import           Taiji.View.Commands
import           Taiji.View.Commands.Network
import           Taiji.View.Commands.Rank
import           Taiji.View.Types

defaultMain :: Options -> IO ()
defaultMain (Options input output cmd) = case cmd of
    ShowRegulTable opt -> mkRegulatoryTable input output opt
    ShowNetwork -> showNetwork input output
    ViewRanks opt -> viewRanks input output opt

main :: IO ()
main = defaultMain =<< execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> header "Taiji-view - a visualization tool for Taiji" )
