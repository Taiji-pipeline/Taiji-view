{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Taiji.View.Commands.Network
    ( showTableParser
    , showNetworkParser
    , mkRegulatoryTable
    , showNetwork
    ) where

import           Bio.Utils.Misc                    (readDouble)
import           Control.Monad
import qualified Data.ByteString.Char8             as B
import           Data.CaseInsensitive              (CI, mk, original)
import           Data.Double.Conversion.ByteString (toShortest)
import           Data.Either                       (either)
import qualified Data.HashMap.Strict               as M
import           Data.List                         (sort)
import           Data.Maybe
import           Data.Monoid                       ((<>))
import           Data.Serialize                    (decode)
import qualified Data.Vector.Unboxed               as U
import qualified Data.Vector.Unboxed.Mutable       as UM
import           IGraph
import           Options.Applicative
import           System.IO

import           Taiji.Types
import           Taiji.View.Types

showTableParser :: Parser Command
showTableParser = fmap ShowRegulTable $ ShowRegulTableOpts
    <$> strArgument
      ( metavar "EXPRESSION_FILE"
     <> help "expression file" )
    <*> switch
      ( long "log2"
     <> help "apply log2 transformatiom" )
    <*> option auto
      ( long "percent"
     <> value 1.0
     <> help "keep only top XX percent genes. (default: 1.0)"
      )
    <*> (optional . option auto)
      ( long "binarize"
     <> help "Binarize edge weights. (default: off)"
      )

showNetworkParser :: Parser Command
showNetworkParser = pure ShowNetwork

showNetwork :: FilePath -> FilePath -> IO ()
showNetwork input output = withFile output WriteMode $ \h -> do
    gr <- either error id . decode <$> B.readFile input :: IO (Graph 'D NetNode NetEdge)
    B.hPutStrLn h "from\tto\tedge_weight\tnode_weight_from\tnode_weight_to"
    forM_ (edges gr) $ \(fr, to) -> do
        let NetEdge{..} = edgeLab gr (fr, to)
        B.hPutStrLn h $ B.intercalate "\t"
            [ original $ nodeName $ nodeLab gr fr
            , original $ nodeName $ nodeLab gr to
            , toShortest $ sqrt $ fromMaybe 0.1 weightExpression * sites
            , toShortest $ exp $ fromMaybe (-10) $ nodeScaledExpression $ nodeLab gr fr
            , toShortest $ exp $ fromMaybe (-10) $ nodeScaledExpression $ nodeLab gr to
            ]

mkRegulatoryTable :: FilePath
                  -> FilePath
                  -> ShowRegulTableOpts
                  -> IO ()
mkRegulatoryTable network_fl output_fl ShowRegulTableOpts{..} = do
    expr <- readExpression expression_fl
    gr <- either error id . decode <$> B.readFile network_fl :: IO (Graph 'D NetNode NetEdge)
    let (tfList, result) = getRegulators logScale percent binarize expr gr
        header = B.intercalate "\t" $ "Target_Gene" : map original tfList
        content = flip map result $ \(xs, (name, y)) -> B.intercalate "\t" $
            original name : toShortest y : map toShortest (U.toList xs)
    B.writeFile output_fl $ B.unlines $ header : content

getRegulators :: Bool
              -> Double
              -> Maybe Double
              -> M.HashMap GeneName Double    -- ^ Gene expressions
              -> Graph 'D NetNode NetEdge
              -> ( [CI B.ByteString]
                 , [(U.Vector Double, (CI B.ByteString, Double))] )
getRegulators logScale percent bin expr gr = (tfs, mapMaybe fn $ nodes gr)
  where
    fn nd | null children || exprGene < cutoff = Nothing
          | otherwise = Just (val, (geneName, transform exprGene))
      where
        geneName = nodeName $ nodeLab gr nd
        exprGene = M.lookupDefault 0 geneName expr
        children = suc gr nd
        val = U.create $ do
            vec <- UM.replicate n 0
            forM_ children $ \x -> do
                let name = nodeName $ nodeLab gr x
                case M.lookup name tfList of
                    Nothing -> return ()
                    Just idx -> do
                        let e = transform $ M.lookupDefault undefined name expr
                            w = sites $ edgeLab gr (nd, x)
                        case bin of
                            Nothing -> UM.unsafeWrite vec idx $ w * e
                            Just b -> when (w >= b) $ UM.unsafeWrite vec idx e
            return vec
    tfs = sort $ filter (\x -> fromMaybe False $ fmap (>= cutoff) $ M.lookup x expr) $
        map (nodeName . nodeLab gr) $ filter (not . null . pre gr) $ nodes gr
    tfList = M.fromList $ zip tfs [0..]
    n = M.size tfList
    transform | logScale = logBase 2 . (+1)
              | otherwise = id
    cutoff = let vs = reverse $ sort $ snd $ unzip $ filter (B.null . snd .
                    B.breakSubstring "UNKNOWN" . original . fst) $ M.toList expr
                 len = truncate $ percent * fromIntegral (length vs)
             in last $ take len vs

-- | Read RNA expression data
readExpression :: FilePath
               -> IO (M.HashMap GeneName Double)
readExpression fl = do
    (_:dat) <- B.lines <$> B.readFile fl
    return $ M.fromList $ flip map dat $ \l ->
        let (f1:f2:_) = B.split '\t' l
        in (mk f1, readDouble f2)
{-# INLINE readExpression #-}
