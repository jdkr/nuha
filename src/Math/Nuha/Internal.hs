-- {-# LANGUAGE BangPatterns #-}

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Nuha.Internal where

-- import Debug.Trace
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V



-- | cartesian product for a list of lists
cartProd :: [[Int]] -> [[Int]]
{-# INLINE cartProd #-}
cartProd mIdcs
    | mIdcs == [] = error "cartProd : Should not happen, empty mIdcs"
    | (length mIdcs == 1) = [ [i] | i <- mIdcs!!0]
    | (length mIdcs == 2) = [ [i1,i2] | i1 <- mIdcs!!0, i2 <- mIdcs!!1]
    | otherwise = [i : indices | i <- mIdcs!!0, indices <- cartProd (tail mIdcs)]

-- | unsafe function for convert an 1d index (of the holor values) to a multiindex of the holor
fromIndexToMultiIndex :: [Int] -> Int -> [Int]
{-# INLINE fromIndexToMultiIndex #-}
fromIndexToMultiIndex strides idx = iterIndices idx 0 where
    iterIndices p i
        | (i+1) < length strides = div : iterIndices mod (i+1)
        | otherwise = [div]
        where
            stride = strides!!i
            (div, mod) = divMod p stride

-- | unsafe function for convert multiindex of the holor to a an 1d index (of the holor values)
fromMultiIndexToIndex
    :: [Int] -- ^ strides
    -> [Int] -- ^ multiindex
    -> Int -- ^ index
{-# INLINE fromMultiIndexToIndex #-}
fromMultiIndexToIndex strides mIdx = sum (zipWith (*) strides mIdx)

-- | calculates the holor strides from the holor shape
fromShapeToStrides :: [Int] -> [Int]
{-# INLINE fromShapeToStrides #-}
fromShapeToStrides shape = [foldr (*) 1 (drop i shape) | i <- [1..length shape]]

-- | calculates all possible multiindices for a holor from its shape
fromShapeToMultiIndices :: [Int] -> [[Int]]
{-# INLINE fromShapeToMultiIndices #-}
fromShapeToMultiIndices shape = mIdcs where
    mIdcs = cartProd ranges
    ranges = [[0..s-1] | s <- shape]

-- | tests if an index is valid
isValidIdx
    :: Int -- ^ length
    -> Int -- ^ idx
    -> Bool
{-# INLINE isValidIdx #-}
isValidIdx len idx = (idx < len) && (idx >= 0)

-- | tests if a multiindex is valid
isValidMIdx
    :: [Int] -- ^ shape
    -> [Int] -- ^ multiindex
    -> Bool
{-# INLINE isValidMIdx #-}
isValidMIdx shp mIdx =
    (length shp == length mIdx) &&
    (and $ fmap (>0) (zipWith (-) shp mIdx)) &&
    (and $ fmap (>=0) mIdx)

-- | tests if multiindices are valid
isValidMIdcs
    :: [Int] -- ^ shape
    -> [[Int]] -- ^ multiindices
    -> Bool
{-# INLINE isValidMIdcs #-}
isValidMIdcs shp mIdcs =
    length mIdcs == length shp &&
    and [and $ fmap (\k -> (0<=k && k < shp!!i) ) (mIdcs!!i) | i<-[0 .. length mIdcs - 1]]

