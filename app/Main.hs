{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Main where

import           Data.Monoid         ((<>))
import           Options.Applicative

import           Taiji.View.Commands.Rank
import Taiji.View.Commands.QC
import           Taiji.View.Types

mainOptParser :: Parser Command
mainOptParser = subparser
    ( command "rank"
        (info (plotRankParser <**> helper) (progDesc "Plot ranks"))
   <> command "qc"
        (info (plotQCParser <**> helper) (progDesc "Plot QC"))
    )

defaultMain :: Command -> IO ()
defaultMain cmd = case cmd of
    ViewRanks{..} -> viewRanks cmd
    ViewQC{..} -> plotQCs cmd

main :: IO ()
main = defaultMain =<< execParser opts
  where
    opts = info (mainOptParser <**> helper)
      ( fullDesc
     <> header "Taiji-view - a visualization tool for Taiji" )