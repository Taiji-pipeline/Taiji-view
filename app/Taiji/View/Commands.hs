module Taiji.View.Commands
    ( parser
    ) where

import           Options.Applicative
import           Data.Monoid         ((<>))

import Taiji.View.Types
import Taiji.View.Commands.Network
import Taiji.View.Commands.Rank

mkParser :: Parser Command -> Parser Options
mkParser p = Options
    <$> strArgument
      ( metavar "INPUT"
     <> help "input file" )
    <*> strArgument
      ( metavar "OUTPUT"
     <> help "output file" )
    <*> p

parser :: Parser Options
parser = subparser
    ( command "table"
        (info (mkParser showTableParser <**> helper) (progDesc "View network as table"))
   <> command "network"
        (info (mkParser showNetworkParser <**> helper) (progDesc "View network"))
   <> command "rank"
        (info (mkParser plotRankParser <**> helper) (progDesc "Plot ranks"))
    )
