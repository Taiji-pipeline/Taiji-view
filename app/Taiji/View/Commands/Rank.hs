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
import qualified Data.HashSet as S
import Data.Function (on)
import AI.Clustering.Hierarchical hiding (normalize)
import Control.Arrow (first)
import           Data.Colour            (blend)
import           Options.Applicative
import qualified Data.Matrix            as M
import qualified Data.Vector            as V
import           Diagrams.Backend.Cairo (B)
import           Diagrams.Backend.Cairo (renderCairo)
import           Diagrams.Prelude       hiding (option, scale, value, normalize)
import           Graphics.SVGFonts      (textSVG)
import Data.Colour.Palette.BrewerSet
import           Statistics.Sample
import Statistics.Correlation (spearman)
import           Text.Printf

import           Taiji.View.Types
import           Taiji.View.Utils hiding (zip, unzip)
import qualified Taiji.View.Utils as U

plotRankParser :: Parser Command
plotRankParser = ViewRanks
    <$> strArgument
      ( metavar "INPUT"
     <> help "rank file" )
    <*> strArgument
      ( metavar "OUTPUT"
     <> help "output file" )
    <*> (optional . strOption)
      ( long "expression" )
    <*> option auto
      ( long "cv"
     <> value 0.5
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

viewRanks :: Command -> IO ()
viewRanks ViewRanks{..} = do
    grp <- case colGroup of
        Nothing -> return Nothing
        Just fl -> fmap (Just . map (T.splitOn ",") . T.lines) $ T.readFile fl

    rowFilt <- case rowNamesFilter of
        Nothing -> return $ const True
        Just fl -> do
            names <- T.lines <$> T.readFile fl
            return (`elem` names)

    df <- case exprFile of
        Just fl -> do
            mat <- cbind .
                map (reorderColumns (orderByCluster fst)) .
                groupDataFrame grp .
                reorderRows (orderByCluster fst) .
                uncurry U.zip . first normalize . U.unzip .
                filterRows (const $ filtCV cv . fst . V.unzip) .
                filterRows (const $ V.any ((>=minRank) . fst)) .
                filterRows (flip $ const rowFilt) <$> readData rankFile fl
            case outputValues of
                Nothing -> return ()
                Just x -> writeTable x (T.pack . show . fst) mat
            return $ Left mat
        Nothing -> do
            mat <- cbind .
                map (reorderColumns (orderByCluster id)) .
                groupDataFrame grp .
                reorderRows (orderByCluster id) .
                normalize .
                filterRows (const $ filtCV cv) .
                filterRows (const $ V.any (>=minRank)) .
                filterRows (flip $ const rowFilt) <$> readTable rankFile
            case outputValues of
                Nothing -> return ()
                Just x -> writeTable x (T.pack . show) mat
            return $ Right mat

    let w = width dia
        h = height dia
        n = fromIntegral (either (M.cols . _dataframe_data)
            (M.cols . _dataframe_data) df) * 50
        dia = bubblePlot df $ BubblePlotOpts 15 reds rankRange
    renderCairo output (dims2D n (n*(h/w))) dia


groupDataFrame :: Maybe [[T.Text]] -> DataFrame a -> [DataFrame a]
groupDataFrame Nothing df = [df]
groupDataFrame (Just grp) df = map (df `csub`) $ grp ++ [others]
  where
    others = S.toList $
        S.fromList (colNames df) `S.difference` S.fromList (concat grp)

orderByCluster :: (a -> Double) -> ReodrderFn a 
orderByCluster f xs = flatten $ hclust Ward (V.fromList xs) dist
  where
    dist = euclidean `on` V.map f . snd

data BubblePlotOpts = BubblePlotOpts
    { _radius :: Double                      -- ^ The size of bubble
    , _colours :: V.Vector (Colour Double)   -- ^ Color scheme
    , _rank_range :: Maybe (Double, Double)        -- ^ The range of ranks
    }

bubblePlot :: Either (DataFrame (Double, Double)) (DataFrame Double)
           -> BubblePlotOpts -> Diagram B
bubblePlot (Left df) opts@BubblePlotOpts{..}
    | null (rowNames df) = error "Nothing to plot"
    | otherwise = center bubbles === strutY 30 === center legend
  where
    bubbles = drawBubbles df{_dataframe_data = M.zip ranks' expr'} opts
    legend = mkLegend opts
        (V.minimum $ M.flatten expr, V.maximum $ M.flatten expr)
        (case _rank_range of
            Nothing -> (V.minimum $ M.flatten ranks, V.maximum $ M.flatten ranks)
            Just r -> r )
    expr' = M.fromVector (M.dim expr) $ linearMap (4, _radius) $ M.flatten expr
    ranks' = M.fromVector (M.dim ranks) $ case _rank_range of
        Nothing -> linearMap (0, 1) $ M.flatten ranks
        Just rng -> linearMapBounded rng (0, 1) $ M.flatten ranks
    (ranks, expr) = M.unzip $ _dataframe_data df
bubblePlot (Right df) opts@BubblePlotOpts{..}
    | null (rowNames df) = error "Nothing to plot"
    | otherwise = center bubbles === strutY 30 === center legend
  where
    bubbles = drawBubbles df{_dataframe_data = M.zip expr' ranks'} opts
    expr' = M.fromVector (M.dim ranks) $ V.replicate (uncurry (*) $ M.dim ranks) 0.7
    ranks' = M.fromVector (M.dim ranks) $ linearMap (4, _radius) $ M.flatten ranks
    ranks = _dataframe_data df
    legend = mkCircleLegend "normalized rank score"
        ( V.minimum $ M.flatten $ _dataframe_data df
        , V.maximum $ M.flatten $ _dataframe_data df) _radius

drawBubbles :: DataFrame (Double, Double)
            -> BubblePlotOpts 
            -> Diagram B
drawBubbles df BubblePlotOpts{..} = vsep 1 $ (colnames:) $
    zipWith drawBubble (rowNames df) $ M.toLists $ M.zip ranks' expr'
  where
    drawBubble lab xs = alignR $ textBounded lab ||| strutX 5 ||| bb ||| cor
      where
        bb = hsep 1 $ flip map xs $ \(x,y) -> withEnvelope unitEnvelope $
            circle y # lw 0 # fc (colorMapSmooth x _colours)
        cor = let r = spearman $ V.fromList xs
              in withEnvelope unitEnvelope $
                    square _radius # lw 0 # fc (colorMapSmooth ((r+1)/2) buYlRd)
    colnames = alignR $ hsep 1 $ map
        (\x -> (alignB $ textBounded x # rotate (90 @@ deg)) <> unitEnvelope) $
        colNames df ++ [""]
    (ranks', expr') = M.unzip $ _dataframe_data df
    unitEnvelope = circle _radius # lw 0

mkLegend :: BubblePlotOpts -> (Double, Double) -> (Double, Double) -> Diagram B
mkLegend BubblePlotOpts{..} (min_expr, max_expr) (min_rank, max_rank) =
    hsep 50 [rank_legend, expr_legend, cor_legend]
  where
    expr_legend = mkCircleLegend "inverse hyperbolic sine of expression level"
        (min_expr, max_expr) _radius
    rank_legend = mkColorLegend "normalized rank score" (min_rank, max_rank) _colours
    cor_legend = vsep 2
        [ rect 100 25 # lw 0 # fillTexture (mkGradient buYlRd)
        , center $ position $ zip [0^&0, 50^&0, 100^&0] $ map textBounded ["-1", "0", "1"]
        , textBounded "Spearman's correlation" ]
{-# INLINE mkLegend #-}

mkColorLegend :: T.Text   -- ^ Title
              -> (Double, Double)  -- ^ lower and upper bound
              -> V.Vector (Colour Double)   -- ^ color scheme
              -> Diagram B
mkColorLegend title (lo, hi) colors = vsep 2 $
    [ rect 100 25 # lw 0 # fillTexture (mkGradient colors)
    , center $ position $ zip [0^&0, 50^&0, 100^&0] $
        map (textBounded . T.pack . printf "%.2f") values
    , textBounded title ]
  where
    values = [lo, lo + (hi - lo) / 2, hi]

mkCircleLegend :: T.Text   -- ^ Title
               -> (Double, Double)     -- ^ Lower and upper bound
               -> Double    -- ^ radius
               -> Diagram B
mkCircleLegend title (lo, hi) r = center
    ( hsep 1 $ map (\(x,s) -> withEnvelope unitEnvelope
    (circle s # lw 0 # fc black) === strutY 2 === textBounded (T.pack $ printf "%.2f" x)) $
    zip values $ V.toList $ linearMap (4, r) $ V.fromList values ) ===
    strutY 2 === textBounded title
  where
    values = [lo, lo + (hi - lo) / 3 .. hi]
    unitEnvelope = circle r # lw 0 :: Diagram B

mkGradient :: V.Vector (Colour Double) -> Texture Double
mkGradient cs = mkLinearGradient stops ((-50) ^& 0) (50 ^& 0) GradPad
  where
    stops = mkStops $ zipWith (\c x -> (c, x, 1)) (V.toList cs) [0, 1/(n-1) .. 1]
    n = fromIntegral $ V.length cs
{-# INLINE mkGradient #-}

-- | Map numbers to colors
colorMapSmooth :: Double -- a value from 0 to 1
               -> V.Vector (Colour Double) -> Colour Double
colorMapSmooth x colors
    | isNaN x = black
    | x <0 || x > 1 = error "input value is out of range."
    | x == 1 = V.last colors
    | n == 2 = blend (1-p) (V.head colors) $ V.last colors
    | otherwise = blend (1 - p) (colors V.! i) $ colors V.! (i+1)
  where
    (i,p) = properFraction $ x * fromIntegral (n - 1)
    n = V.length colors
{-# INLINE colorMapSmooth #-}

textBounded :: T.Text -> Diagram B
textBounded x = stroke (textSVG (T.unpack x) 15) # lw 0 # fc black
{-# INLINE textBounded #-}

linearMap :: (Double, Double) -> V.Vector Double -> V.Vector Double
linearMap (lo, hi) xs = V.map f xs
  where
    f x = lo + (x - min') / (max' - min') * (hi - lo)
    min' = V.minimum xs
    max' = V.maximum xs
{-# INLINE linearMap #-}

linearMapBounded :: (Double, Double)    -- ^ Range of input data
                 -> (Double, Double)    -- ^ Range of output
                 -> V.Vector Double
                 -> V.Vector Double
linearMapBounded (min', max') (lo, hi) xs = V.map f xs
  where
    f x | x <= min' = lo
        | x >= max' = hi
        | otherwise = lo + (x - min') / (max' - min') * (hi - lo)
{-# INLINE linearMapBounded #-}

-------------------------------------------------------------------------------
-- Colours
-------------------------------------------------------------------------------

buYlRd :: V.Vector (Colour Double)
buYlRd = V.fromList $ reverse $ brewerSet RdYlBu 9

reds :: V.Vector (Colour Double)
reds = V.fromList [white, red]

-- | Normalize the data frame.
normalize :: DataFrame Double -> DataFrame Double
normalize = mapRows f
  where
    f xs | V.length xs <= 2 = V.map (logBase 2 . (/ V.head xs)) xs
         | otherwise = scale xs

-- | Determine whether the input pass the CV cutoff
filtCV :: Double -> V.Vector Double -> Bool
filtCV cutoff xs = sqrt v / m >= cutoff
  where
    (m, v) = meanVarianceUnb xs

-- | Determine whether the input pass the fold-change cutoff
filtFC :: Double -> V.Vector Double -> Bool
filtFC cutoff xs = V.maximum xs / V.minimum xs >= cutoff
