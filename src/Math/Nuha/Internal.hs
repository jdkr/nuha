{-# OPTIONS_HADDOCK hide, ignore-exports #-}

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Nuha.Internal where

import Data.Vector.Unboxed (Vector, Unbox, (!))
import qualified Data.Vector.Unboxed as V

import Math.Nuha.Types


cartProd :: [[Int]] -> [[Int]]
{-# INLINE cartProd #-}
-- ^ cartesian product for a list of lists
cartProd mIdcs
    | (length mIdcs == 1) = [ [i] | i <- mIdcs!!0]
    | (length mIdcs == 2) = [ [i1,i2] | i1 <- mIdcs!!0, i2 <- mIdcs!!1]
    | otherwise = [i : indices | i <- mIdcs!!0, indices <- cartProd (tail mIdcs)]

fromIndexToMultiIndex :: [Int] -> Int -> [Int]
{-# INLINE fromIndexToMultiIndex #-}
-- ^ unsafe function for convert an 1d index (of the array values) to a multiindex of the array
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
-- ^ unsafe function for convert multiindex of the array to a an 1d index (of the array values)
fromMultiIndexToIndex strides mIdx = sum (zipWith (*) strides mIdx)

fromShapeToStrides :: [Int] -> [Int]
{-# INLINE fromShapeToStrides #-}
-- ^ calculates the arrays strides from array shape
fromShapeToStrides shape = [foldr (*) 1 (drop i shape) | i <- [1..length shape]]

fromShapeToMultiIndices :: [Int] -> [[Int]]
{-# INLINE fromShapeToMultiIndices #-}
-- ^ calculates all possible multiindices for an array from its shape
fromShapeToMultiIndices shape = mIdcs where
    mIdcs = cartProd ranges
    ranges = [[0..s-1] | s <- shape]

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
    and [and $ fmap (\k -> (0<=k && k<shp!!i) ) (mIdcs!!i) | i<-[0 .. length mIdcs - 1]]

isValidArray :: Unbox a => Array a -> Bool
{-# INLINE isValidArray #-}
-- ^ tests whether an array is instanced correctly, i.e. shape, strides and values fit together
isValidArray array = V.length (aValues array) == product (aShape array)
    && fromShapeToStrides (aShape array) == aStrides array
