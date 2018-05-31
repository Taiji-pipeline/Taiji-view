{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Taiji.View.Utils
    ( Table(..)
    , mapRows
    , filterRows
    , readData
    , writeTable
    ) where

import           Bio.Utils.Functions    (ihs')
import           Bio.Utils.Misc         (readDouble)
import qualified Data.ByteString.Char8  as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.CaseInsensitive   as CI
import           Data.Function          (on)
import           Data.List
import qualified Data.Matrix            as M
import qualified Data.Vector            as V
import qualified Data.HashMap.Strict    as HM

data Table a = Table
    { rowNames :: [T.Text]
    , colNames :: [T.Text]
    , matrix   :: M.Matrix a
    } deriving (Show)

mapRows :: (V.Vector a -> V.Vector b) -> Table a -> Table b
mapRows fn table = table { matrix = matrix' }
  where
    matrix' = M.fromRows $ map fn $ M.toRows $ matrix table

filterRows :: (T.Text -> V.Vector a -> Bool) -> Table a -> Table a
filterRows fn table = table { rowNames = names, matrix = M.fromRows rows }
  where
    (names, rows) = unzip $ filter (uncurry fn) $ zip (rowNames table) $ M.toRows $
        matrix table

type ReodrderFn a = [(T.Text, V.Vector a)] -> [(T.Text, V.Vector a)]

reorderRows :: ReodrderFn a -> Table a -> Table a
reorderRows fn table = table
    { rowNames = names
    , matrix = M.fromRows rows }
  where
    (names, rows) = unzip $ fn $ zip (rowNames table) $ M.toRows $ matrix table

reorderColumns :: ReodrderFn a -> Table a -> Table a
reorderColumns fn table = table
    { colNames = names
    , matrix = M.fromColumns cols}
  where
    (names, cols) = unzip $ fn $ zip (colNames table) $ M.toColumns $ matrix table

-- | Read data, normalize and calculate p-values.
readData :: FilePath   -- ^ PageRank
         -> FilePath   -- ^ Gene expression
         -> IO (Table (Double, Double))  -- ^ ranks, expression
readData input1 input2 = do
    rank <- readTSV <$> B.readFile input1

    -- Read expression profile and apply "ihs" transformation
    expr <- (fmap ihs' . readTSV) <$> B.readFile input2

    let (labels, xs) = unzip $ map unzip $ groupBy ((==) `on` (fst.fst)) $ sort $
            HM.toList $ HM.intersectionWith (,) rank expr
        rowlab = map (T.pack . B.unpack . CI.original) $ fst $ unzip $ map head labels
        collab = map (T.pack . B.unpack . CI.original) $ snd $ unzip $ head labels
    return $ Table rowlab collab $ M.fromLists xs

writeTable :: FilePath -> (a -> T.Text) -> Table a -> IO ()
writeTable output f Table{..} = T.writeFile output $ T.unlines $
    map (T.intercalate "\t") $ ("" : colNames) :
    zipWith (:) rowNames ((map . map) f $ M.toLists matrix)

readTSV :: B.ByteString -> HM.HashMap (CI.CI B.ByteString, CI.CI B.ByteString) Double
readTSV input = HM.fromList $ concatMap (f . B.split '\t') content
  where
    f (x:xs) = zipWith (\s v -> ((CI.mk x, CI.mk s), readDouble v)) samples xs
    (header:content) = B.lines input
    samples = tail $ B.split '\t' header

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