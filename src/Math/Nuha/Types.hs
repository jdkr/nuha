{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Nuha.Types where


import Data.Vector.Unboxed (Vector)

-- | Datatype for a multidimensional array
data Array a = Array
    { aShape :: ![Int] -- ^ Shape of the array. The dimension is the length of the shape
    , aStrides :: ![Int] -- ^ Step sizes for each dimension, needed for indexing
    , aValues :: !(Vector a)  -- ^ Values of the array in row-major order
}


