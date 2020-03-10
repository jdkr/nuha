{-# OPTIONS_HADDOCK hide, ignore-exports #-}
-- {-# LANGUAGE BangPatterns #-} -- for Debug.Trace

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Nuha.Instances where

import Data.Vector.Unboxed (Unbox, (!))
import qualified Data.Vector.Unboxed as V
-- import qualified Debug.Trace as D

import Math.Nuha.Types
import Math.Nuha.Internal


-- TODO: improve formatting
instance (Show a, Unbox a) => Show (Array a) where
    show (Array shape strides values)
        | length shape == 1 = showVector 0 (V.length values)
        | length shape == 2 = showMatrix 0 lenMatrix
        | otherwise = showMatrices 0 lenMatrix
        where
            [i,j] = drop (length shape - 2) shape
            lenMatrix = i*j
            showVector start end
                | start < end =
                    "  " ++ show (values!start) ++
                    showVector (start+1) end
                | otherwise = id "\n"

            showMatrix start end
                | start < end =
                    showVector start (start+j) ++
                    showMatrix (start+j) end
                | otherwise = id ""

            showMatrices start end
                | start < end =
                    id $ (take (length subIndicesStr - 1) subIndicesStr)++ id ",:,:] =" ++ "\n" ++
                    showMatrix start (start+lenMatrix) ++
                    showMatrices (start+lenMatrix) lenValues
                | otherwise = id ""
                where
                    subIndices = take (length shape - 2) (fromIndexToMultiIndex strides start)
                    subIndicesStr = show subIndices
                    -- !subarrayIndices_ = trace ("subIndices: " ++ show subIndices) ()
                    lenValues = V.length values
                    -- !lenValues_ = trace ("lenValues: " ++ show lenValues) ()


instance (Eq a, Unbox a) => Eq (Array a) where
    a1 == a2 = (aShape a1 == aShape a2 && aStrides a1 == aStrides a2 && aValues a1 == aValues a2)
    {-# INLINE (==) #-}
    a1 /= a2 = not $ a1 == a2
    {-# INLINE (/=) #-}


