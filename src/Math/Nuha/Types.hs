-- {-# LANGUAGE BangPatterns #-} -- for Debug.Trace

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Nuha.Types where


import Data.Vector.Unboxed (Vector, Unbox, (!))
import qualified Data.Vector.Unboxed as V
-- import Foreign.Storable (Storable, sizeOf)
-- import qualified Debug.Trace as D

-- import Math.Nuha.Base (sizeOfElems)
import Math.Nuha.Internal


-- | Datatype for a multidimensional array (holor), see https://en.wikipedia.org/wiki/Parry_Moon#Holors:
data Holor a = Holor
    { hShape :: ![Int] -- ^ Shape of the holor. The dimension is the length of the shape
    , hStrides :: ![Int] -- ^ Step sizes for each dimension, needed for indexing
    , hValues :: !(Vector a)  -- ^ Values of the holor in row-major order
}

-- | 2-tuple
type T2 a  = (a, a)
-- | 3-tuple
type T3 a  = (a, a, a)
-- | 4-tuple
type T4 a  = (a, a, a, a)
-- | 2,2-tuple
type T22 a = T2 (T2 a)
-- | 3,2-tuple
type T32 a = T3 (T2 a)
-- | 4,2-tuple
type T42 a = T4 (T2 a)
-- | 2,3-tuple
type T23 a = T2 (T3 a)
-- | 3,3-tuple
type T33 a = T3 (T3 a)
-- | 4,3-tuple
type T43 a = T4 (T3 a)
-- | 2,4-tuple
type T24 a = T2 (T4 a)
-- | 3,4-tuple
type T34 a = T3 (T4 a)
-- | 4,4-tuple
type T44 a = T4 (T4 a)



-- TODO: improve formatting
instance (Show a, Unbox a) => Show (Holor a) where
    show (Holor shape strides values)
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
                    -- !subIndices_ = trace ("subIndices: " ++ show subIndices) ()
                    subIndicesStr = show subIndices
                    lenValues = V.length values
                    -- !lenValues_ = trace ("lenValues: " ++ show lenValues) ()


instance (Eq a, Unbox a) => Eq (Holor a) where
    {-# INLINE (==) #-}
    h1 == h2 = (hShape h1 == hShape h2 && hStrides h1 == hStrides h2 && hValues h1 == hValues h2)

    {-# INLINE (/=) #-}
    h1 /= h2 = not $ h1 == h2

-- | Ordering is only relevant on the holor values (i.e. independent of shape)
instance (Ord a, Unbox a) => Ord (Holor a) where
    {-# INLINE compare #-}
    compare h1 h2 = compare (hValues h1) (hValues h2)

    {-# INLINE (<) #-}
    h1 < h2 = (hValues h1) < (hValues h2)

    {-# INLINE (<=) #-}
    h1 <= h2 = (hValues h1) <= (hValues h2)

    {-# INLINE (>) #-}
    h1 > h2 = (hValues h1) > (hValues h2)

    {-# INLINE (>=) #-}
    h1 >= h2 = (hValues h1) >= (hValues h2)


{- TODO:
instance Storable a => Storable (Holor a) where
    sizeOfInt = 8
    sizeOfValues = sizeOfElems hlr

    {-# INLINE sizeOf #-}
    -- Size as size of holor values + size of dimension variable + size of shape information
    sizeOf hlr = sizeOfValues + sizeOfInt + (dim hlr) * sizeOfInt

    {-# INLINE alignment #-}
    alignment _ = alignment (undefined::a)

    {-# INLINE peek #-}
    peek ptr1 = do
        let ptr2 = castPtr ptr1 :: Ptr a
        valuesPtr <- peek ptr2
        dimPtr <- peekByteOff ptr2 (sizeOfValues)
        shapePtr <- peekByteOff ptr2 (sizeOfValues + dim * sizeOfInt)


        let values = peekArray
        let strides = frohShapeToStrides shape
        return $ Holor shape strides values

    {-# INLINE poke #-}
    poke ptr1 (Holor shape strides values) = do
        let ptr2 = castPtr ptr1 :: Ptr a

-}

