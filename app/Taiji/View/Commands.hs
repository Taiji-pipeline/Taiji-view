module Taiji.View.Commands
    ( parser
    ) where

import           Options.Applicative
import           Data.Monoid         ((<>))

import Taiji.View.Types
import Taiji.View.Commands.Network
import Taiji.View.Commands.Ranks

mkParser :: Parser Command -> Parser Options
mkParser p = Options
    <$> strArgument
      ( metavar "NETWORK_FILE"
     <> help "network file" )
    <*> strArgument
      ( metavar "OUTPUT"
     <> help "output file" )
    <*> p

parser :: Parser Options
parser = subparser
    ( command "table"
        (info (mkParser showTableParser) (progDesc "View network as table"))
   <> command "network"
        (info (mkParser showNetworkParser) (progDesc "View network"))
   <> command "rank"
        (info (mkParser plotRankParser) (progDesc "Plot ranks"))
    )
