{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Taiji.View.Utils
    ( DataFrame(..)
    , DataFrameIndex(..)
    , cbind
    , rowNames
    , colNames
    , ReodrderFn
    , reorderColumns
    , reorderRows
    , mapRows
    , filterRows
    , Taiji.View.Utils.zip
    , Taiji.View.Utils.unzip
    , readData
    , writeTable
    , readTable
    ) where

import           Bio.Utils.Functions    (ihs')
import           Bio.Utils.Misc         (readDouble)
import qualified Data.ByteString.Char8  as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.CaseInsensitive   as CI
import           Data.Function          (on)
import           Data.List as L
import qualified Data.Matrix            as M
import qualified Data.Vector            as V
import qualified Data.HashMap.Strict    as HM

data DataFrame a = DataFrame
    { _dataframe_row_names :: V.Vector T.Text
    , _dataframe_row_names_idx :: HM.HashMap T.Text Int
    , _dataframe_col_names :: V.Vector T.Text
    , _dataframe_col_names_idx :: HM.HashMap T.Text Int
    , _dataframe_data :: M.Matrix a
    } deriving (Show)

mkDataFrame :: [T.Text]     -- ^ row names
            -> [T.Text]     -- ^ col names
            -> [[a]]        -- ^ data
            -> DataFrame a
mkDataFrame r c d = DataFrame
    { _dataframe_row_names = V.fromList r
    , _dataframe_row_names_idx = HM.fromList $ L.zip r [0..]
    , _dataframe_col_names = V.fromList c
    , _dataframe_col_names_idx = HM.fromList $ L.zip c [0..]
    , _dataframe_data = M.fromLists d }

class DataFrameIndex i where
    csub :: DataFrame a -> [i] -> DataFrame a

instance DataFrameIndex Int where
    csub df idx = df
        { _dataframe_col_names = V.fromList col_names
        , _dataframe_col_names_idx = HM.fromList $ L.zip col_names [0..]
        , _dataframe_data = M.fromColumns $ map (_dataframe_data df `M.takeColumn`) idx }
      where
        col_names = map (_dataframe_col_names df V.!) idx

instance DataFrameIndex T.Text where
    csub df idx = csub df idx'
      where
        idx' = map (\i -> HM.lookupDefault
            (error $ "index doesn't exist: " ++ T.unpack i) i $
            _dataframe_col_names_idx df) idx

cbind :: [DataFrame a] -> DataFrame a
cbind dfs | allTheSame (map _dataframe_row_names dfs) = DataFrame
    { _dataframe_row_names = row_names
    , _dataframe_row_names_idx = HM.fromList $ L.zip (V.toList row_names) [0..]
    , _dataframe_col_names = col_names
    , _dataframe_col_names_idx = HM.fromList $ L.zip (V.toList col_names) [0..]
    , _dataframe_data = M.fromBlocks undefined [map _dataframe_data dfs] }
          | otherwise = error "Row names differ"
  where
    allTheSame xs = all (== head xs) (tail xs)
    row_names = V.concat $ map (_dataframe_row_names) dfs
    col_names = V.concat $ map (_dataframe_col_names) dfs

zip :: DataFrame a -> DataFrame b -> DataFrame (a,b)
zip df1 df2
    | _dataframe_col_names df1 == _dataframe_col_names df2 &&
      _dataframe_row_names df1 == _dataframe_row_names df2 = df1
        {_dataframe_data = M.zip (_dataframe_data df1) $ _dataframe_data df2}
    | otherwise = error "names mismatch"

unzip :: DataFrame (a,b) -> (DataFrame a, DataFrame b)
unzip df = (df{_dataframe_data = a}, df{_dataframe_data = b})
  where
    (a,b) = M.unzip $ _dataframe_data df

rowNames :: DataFrame a -> [T.Text]
rowNames DataFrame{..} = V.toList _dataframe_row_names

colNames :: DataFrame a -> [T.Text]
colNames DataFrame{..} = V.toList _dataframe_col_names

mapRows :: (V.Vector a -> V.Vector b) -> DataFrame a -> DataFrame b
mapRows fn df = df
    { _dataframe_data = M.fromRows $ map fn $ M.toRows $ _dataframe_data df }

filterRows :: (T.Text -> V.Vector a -> Bool) -> DataFrame a -> DataFrame a
filterRows fn df = df
    { _dataframe_row_names = V.fromList names
    , _dataframe_data = M.fromRows rows }
  where
    (names, rows) = L.unzip $ filter (uncurry fn) $
        L.zip (V.toList $ _dataframe_row_names df) $ M.toRows $ _dataframe_data df

type ReodrderFn a = [(T.Text, V.Vector a)] -> [(T.Text, V.Vector a)]

reorderRows :: ReodrderFn a -> DataFrame a -> DataFrame a
reorderRows fn df = df
    { _dataframe_row_names = V.fromList names
    , _dataframe_data = M.fromRows rows }
  where
    (names, rows) = L.unzip $ fn $ L.zip (V.toList $ _dataframe_row_names df) $
        M.toRows $ _dataframe_data df

reorderColumns :: ReodrderFn a -> DataFrame a -> DataFrame a
reorderColumns fn df = df
    { _dataframe_col_names = V.fromList names
    , _dataframe_data = M.fromColumns cols}
  where
    (names, cols) = L.unzip $ fn $ L.zip (V.toList $ _dataframe_col_names df) $
        M.toColumns $ _dataframe_data df

writeTable :: FilePath -> (a -> T.Text) -> DataFrame a -> IO ()
writeTable output f DataFrame{..} = T.writeFile output $ T.unlines $
    map (T.intercalate "\t") $ ("" : V.toList _dataframe_col_names) :
    L.zipWith (:) (V.toList _dataframe_row_names)
    ((map . map) f $ M.toLists _dataframe_data)

-- | Read data, normalize and calculate p-values.
readData :: FilePath   -- ^ PageRank
         -> FilePath   -- ^ Gene expression
         -> IO (DataFrame (Double, Double))  -- ^ ranks, expression
readData input1 input2 = do
    rank <- readTSV <$> B.readFile input1

    -- Read expression profile and apply "ihs" transformation
    expr <- (fmap ihs' . readTSV) <$> B.readFile input2

    let (labels, xs) = L.unzip $ map L.unzip $ groupBy ((==) `on` (fst.fst)) $ sort $
            HM.toList $ HM.intersectionWith (,) rank expr
        rowlab = map (T.pack . B.unpack . CI.original) $ fst $ L.unzip $ map head labels
        collab = map (T.pack . B.unpack . CI.original) $ snd $ L.unzip $ head labels
    return $ mkDataFrame rowlab collab xs

readTSV :: B.ByteString -> HM.HashMap (CI.CI B.ByteString, CI.CI B.ByteString) Double
readTSV input = HM.fromList $ concatMap (f . B.split '\t') content
  where
    f (x:xs) = L.zipWith (\s v -> ((CI.mk x, CI.mk s), readDouble v)) samples xs
    (header:content) = B.lines input
    samples = tail $ B.split '\t' header

readTable :: FilePath -> IO (DataFrame Double)
readTable input = do
    (header:content) <- B.lines <$> B.readFile input
    let samples = tail $ B.split '\t' header
        (rows, dat) = L.unzip $ map ((\(x:xs) ->
            (T.pack $ B.unpack x, map readDouble xs)) . B.split '\t') content
    return $ mkDataFrame rows (map (T.pack . B.unpack) samples) dat

    {-
orderByName :: [T.Text] -> ReodrderFn a
orderByName prefix = sortBy $ \(a,_) (b,_) ->
    let idx1 = findIdx a
        idx2 = findIdx b
    in case () of
        _ | isJust idx1 && isJust idx2 -> case compare idx1 idx2 of
                LT -> LT
                GT -> GT
                EQ -> compare a b
          | otherwise -> compare a b
  where
    findIdx x = go prefix 0
      where
        go (y:ys) !i | isPrefixOf y x = Just i
                     | otherwise = go ys (i+1)
        go _ _ = Nothing
        -}