-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Nuha.Internal where

import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V



cartProd :: [[Int]] -> [[Int]]
{-# INLINE cartProd #-}
-- ^ cartesian product for a list of lists
cartProd mIdcs
    | (length mIdcs == 1) = [ [i] | i <- mIdcs!!0]
    | (length mIdcs == 2) = [ [i1,i2] | i1 <- mIdcs!!0, i2 <- mIdcs!!1]
    | otherwise = [i : indices | i <- mIdcs!!0, indices <- cartProd (tail mIdcs)]

fromIndexToMultiIndex :: [Int] -> Int -> [Int]
{-# INLINE fromIndexToMultiIndex #-}
-- ^ unsafe function for convert an 1d index (of the holor values) to a multiindex of the holor
fromIndexToMultiIndex strides idx = iterIndices idx 0 where
    iterIndices p i
        | (i+1) < length strides = div : iterIndices mod (i+1)
        | otherwise = [div]
        where
            stride = strides!!i
            (div, mod) = divMod p stride

fromMultiIndexToIndex
    :: [Int] -- ^ strides
    -> [Int] -- ^ multiindex
    -> Int -- ^ index
{-# INLINE fromMultiIndexToIndex #-}
-- ^ unsafe function for convert multiindex of the holor to a an 1d index (of the holor values)
fromMultiIndexToIndex strides mIdx = sum (zipWith (*) strides mIdx)

frohShapeToStrides :: [Int] -> [Int]
{-# INLINE frohShapeToStrides #-}
-- ^ calculates the holors strides from holor shape
frohShapeToStrides shape = [foldr (*) 1 (drop i shape) | i <- [1..length shape]]

frohShapeToMultiIndices :: [Int] -> [[Int]]
{-# INLINE frohShapeToMultiIndices #-}
-- ^ calculates all possible multiindices for a holor from its shape
frohShapeToMultiIndices shape = mIdcs where
    mIdcs = cartProd ranges
    ranges = [[0..s-1] | s <- shape]

isValidIdx
    :: Int -- ^ length
    -> Int -- ^ idx
    -> Bool
{-# INLINE isValidIdx #-}
-- ^ tests whether an index is valid
isValidIdx len idx = (idx < len) && (idx >= 0)

isValidMIdx
    :: [Int] -- ^ shape
    -> [Int] -- ^ multiindex
    -> Bool
{-# INLINE isValidMIdx #-}
-- ^ tests whether a multiindex is valid
isValidMIdx shp mIdx =
    (length shp == length mIdx) &&
    (and $ fmap (>0) (zipWith (-) shp mIdx)) &&
    (and $ fmap (>=0) mIdx)

isValidMIdcs
    :: [Int] -- ^ shape
    -> [[Int]] -- ^ multiindices
    -> Bool
{-# INLINE isValidMIdcs #-}
-- ^ tests whether multiindices are valid
isValidMIdcs shp mIdcs =
    length mIdcs == length shp &&
    and [and $ fmap (\k -> (0<=k && k < shp!!i) ) (mIdcs!!i) | i<-[0 .. length mIdcs - 1]]

