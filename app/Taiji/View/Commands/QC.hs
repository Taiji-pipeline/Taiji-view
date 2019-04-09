{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Taiji.View.Commands.QC where

import           Options.Applicative
import qualified Data.Text as T
import Data.Serialize (decode)
import qualified Data.ByteString as B
import qualified Language.R                        as R
import           Language.R.QQ
import Shelly hiding (FilePath)

import           Taiji.View.Types
import           Taiji.Types

plotQCParser :: Parser Command
plotQCParser = ViewQC
    <$> strArgument
      ( metavar "INPUT"
     <> help "QC file" )
    <*> strArgument
      ( metavar "OUTPUT"
     <> help "output file" )
 
-- | Visualize QCs.
plotQCs :: Command -> IO ()
plotQCs ViewQC{..} = do
    qc <- decode <$> B.readFile qcFile >>= \case
        Right qc -> return qc
        Left err -> error err
    shelly $ mkdir_p $ fromText $ T.pack outputDir
    mapM_ (plotQC outputDir) (qc :: [QC])

plotQC :: FilePath  -- ^ dir
       -> QC
       -> IO ()
plotQC dir qc = case _qc_plot_type qc of
    Bar -> barPlot output name labels vec
    Density -> linePlot output name (map read labels) vec
    _ -> return ()
  where
    QCVector labels vec = _qc_result qc
    output = dir ++ "/QC_" ++ name ++ ".pdf"
    name = _qc_name qc

linePlot :: FilePath
         -> String
         -> [Double]
         -> [Double]
         -> IO ()
linePlot output title xs ys = R.runRegion $ do
    [r| library(ggplot2)
        df <- data.frame(xs=xs_hs, ys=ys_hs)
        p <- ggplot(df, aes(xs, ys)) +
            geom_line() +
            scale_x_continuous(breaks = c(0, 146, 200, 400, 600, 800, 1000)) +
            scale_y_log10() +
            theme_bw() +
            theme(text=element_text(family="Helvetica")) +
            ggtitle(title_hs) +
            ylab("") + xlab("")
        ggsave(output_hs, p)
    |]
    return ()

        
-- | Make bar plots.
barPlot :: FilePath 
        -> String   -- ^ Title
        -> [String] -- ^ Labels
        -> [Double] -- ^ values
        -> IO ()
barPlot output title labels values = R.runRegion $ do
    [r| library(ggplot2)
        df <- data.frame(label=labels_hs, value=values_hs)
        p <- ggplot(df, aes(label, value)) +
            geom_col() +
            theme_bw() +
            theme(text=element_text(family="Helvetica")) +
            ggtitle(title_hs) +
            ylab("") + xlab("")
        ggsave(output_hs, p)
    |]
    return ()


