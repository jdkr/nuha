{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}  -- for Debug.Trace

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Nuha.Base where

import qualified Prelude as P
import Prelude hiding (map, replicate, (!!))
import Control.Monad (zipWithM_)
-- import qualified Debug.Trace as D
import Foreign.Storable (Storable, sizeOf)
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import Math.Nuha.Types
import Math.Nuha.Internal


-- Naming conventions for indices:
-- idx : index for hValues :: Int
-- idcs : list of indices for hValues :: [Int]
-- mIdx : multiindex for a holor :: [Int]
-- mIdcs : list of multiindices for a holor :: [[Int]]


{- Some usefull links:
https://cheatsheets.quantecon.org/
http://hyperpolyglot.org/numerical-analysis
https://docs.julialang.org/en/v1/manual/arrays/
https://github.com/Magalame/fastest-matrices (benchmarks)
-}

{- Why unboxed immutable Vectors? :
https://stackoverflow.com/questions/34692809/lists-boxed-vectors-and-unboxed-vectors-for-heavy-scientific-computations
https://github.com/lehins/massiv
https://stackoverflow.com/ questions/40176678/differences-between-storable-and-unboxed-vectors
https://www.schoolofhaskell.com/user/commercial/content/vector
-}


-------------------
-- ** Constructions

holor :: Unbox a => [Int] -> [a] -> Holor a
{-# INLINE holor #-}
{- ^ Basic function for creating a holor

>>> holor [2,2] [1,2,3,4]
  1.0  2.0
  3.0  4.0
>>> holor [2,2] [1,2,3,4] :: Holor Int
  1  2
  3  4
>>> holor [2,2,4] [1 .. 16]
[0,:,:] =
  1.0  2.0  3.0  4.0
  5.0  6.0  7.0  8.0
[1,:,:] =
  9.0  10.0  11.0  12.0
  13.0  14.0  15.0  16.0

-}
holor shape values
    | length shape < 2 = error $ "holor : a holor must have at least a dimension of 2"
    | foldr (*) 1 shape /= length values = error $ "holor : length of values "
        ++ show (length values) ++ " doesn't match to shape " ++ show shape
    | otherwise = Holor shape (frohShapeToStrides shape) (V.fromList values)

vector :: Unbox a => [a] -> Holor a
{-# INLINE vector #-}
{- ^ Convenience function for creating a column-vector (2d-holor with shape [n,1])

>>> vector [1,2,3,4]
 1.0
 2.0
 3.0
 4.0
-}
vector values = Holor [length values, 1] [1, 1] (V.fromList values)

matrix :: Unbox a => [[a]] -> Holor a
{-# INLINE matrix #-}
{- ^ Convenience function for creating a matrix (2d-holor)

>>> matrix [[1,2],[3,4]]
  1.0  2.0
  3.0  4.0
-}
matrix valuesList
    | lengths1dAreEqual = Holor shape (frohShapeToStrides shape) (V.fromList values)
    | otherwise = error $ "matrix : shape mismatch in nested list"
    where
        lengths1d = [length v1 | v1 <- valuesList]
        lengths1dAreEqual = all (== head lengths1d) (tail lengths1d)
        values = [entry | row <- valuesList , entry <- row]
        shape = [length valuesList, head lengths1d]

vector2 :: Unbox a => T2 a -> Holor a
-- ^ Creates a 2x1 holor from a tuple
vector2 (t1,t2) = Holor [2,1] [1,1] $ V.fromList [t1,t2]

vector3 :: Unbox a => T3 a -> Holor a
-- ^ Creates a 3x1 holor from a tuple
vector3 (t1,t2,t3) = Holor [3,1] [1,1] $ V.fromList [t1,t2,t3]

vector4 :: Unbox a => T4 a -> Holor a
-- ^ Creates a 4x1 holor from a tuple
vector4 (t1,t2,t3,t4) = Holor [4,1] [1,1] $ V.fromList [t1,t2,t3,t4]

matrix22 :: Unbox a => T22 a -> Holor a
-- ^ Creates a 2x2 holor from nested tuples
matrix22 ((t11,t12),(t21,t22)) =
    Holor [2,2] [2,1] $ V.fromList [t11,t12,t21,t22]

matrix32 :: Unbox a => T32 a -> Holor a
-- ^ Creates a 3x2 holor from nested tuples
matrix32 ((t11,t12),(t21,t22),(t31,t32)) =
    Holor [3,2] [2,1] $ V.fromList [t11,t12,t21,t22,t31,t32]

matrix42 :: Unbox a => T42 a -> Holor a
-- ^ Creates a 4x2 holor from nested tuples
matrix42 ((t11,t12),(t21,t22),(t31,t32),(t41,t42)) =
    Holor [4,2] [2,1] $ V.fromList [t11,t12,t21,t22,t31,t32,t41,t42]

matrix23 :: Unbox a => T23 a -> Holor a
-- ^ Creates a 2x3 holor from nested tuples
matrix23 ((t11,t12,t13),(t21,t22,t23)) =
    Holor [2,3] [3,1] $ V.fromList [t11,t12,t13,t21,t22,t23]

matrix33 :: Unbox a => T33 a -> Holor a
-- ^ Creates a 3x3 holor from nested tuples
matrix33 ((t11,t12,t13),(t21,t22,t23),(t31,t32,t33)) =
    Holor [3,3] [3,1] $ V.fromList [t11,t12,t13,t21,t22,t23,t31,t32,t33]

matrix43 :: Unbox a => T43 a -> Holor a
-- ^ Creates a 4x3 holor from nested tuples
matrix43 ((t11,t12,t13),(t21,t22,t23),(t31,t32,t33),(t41,t42,t43)) =
    Holor [4,3] [3,1] $ V.fromList [t11,t12,t13,t21,t22,t23,t31,t32,t33,t41,t42,t43]

matrix24 :: Unbox a => T24 a -> Holor a
-- ^ Creates a 2x4 holor from nested tuples
matrix24 ((t11,t12,t13,t14),(t21,t22,t23,t24)) =
    Holor [2,4] [4,1] $ V.fromList [t11,t12,t13,t14,t21,t22,t23,t24]

matrix34 :: Unbox a => T34 a -> Holor a
-- ^ Creates a 3x4 holor from nested tuples
matrix34 ((t11,t12,t13,t14),(t21,t22,t23,t24),(t31,t32,t33,t34)) =
    Holor [3,4] [4,1] $ V.fromList [t11,t12,t13,t14,t21,t22,t23,t24,t31,t32,t33,t34]

matrix44 :: Unbox a => T44 a -> Holor a
-- ^ Creates a 4x4 holor from nested tuples
matrix44 ((t11,t12,t13,t14),(t21,t22,t23,t24),(t31,t32,t33,t34),(t41,t42,t43,t44)) = Holor
    [4,4] [4,1] $ V.fromList [t11,t12,t13,t14,t21,t22,t23,t24,t31,t32,t33,t34,t41,t42,t43,t44]


replicate :: Unbox a => [Int] -> a -> Holor a
{-# INLINE replicate #-}
{- ^ Creates a holor for a given shape with all entries the same

>>> replicate [1,3] 0
  0.0  0.0  0.0
-}
replicate shape value = holor shape (P.replicate (product shape) value)


----------------
-- ** Properties

shape :: Unbox a => Holor a -> [Int]
{-# INLINE shape #-}
-- ^ Shape of a holor
shape = hShape

numElems :: Unbox a => Holor a -> Int
{-# INLINE numElems #-}
-- ^ Number of holor entrys
numElems hlr = product (shape hlr)

sizeOfElems :: (Unbox a, Storable a) => Holor a -> Int
{-# INLINE sizeOfElems #-}
-- ^ Size of all holor elements in bytes (not equal to consumed memory of the holor)
sizeOfElems hlr = (numElems hlr) * (sizeOfElem hlr)

sizeOfElem :: forall a . (Unbox a, Storable a) => Holor a -> Int
{-# INLINE sizeOfElem #-}
-- ^ Size of a single holor element in bytes
sizeOfElem hlr = sizeOf (undefined::a) -- requires "ScopedTypeVariables" and "forall a"

dim :: Unbox a => Holor a -> Int
{-# INLINE dim #-}
{- ^ Dimension of a holor (length of shape)

>>> dim $ holor [2,2,2] [0..7]
3
-}
dim = length . hShape


---------------
-- ** Accessors

infixl 9 !
infixl 9 !!
infixl 9 |!
infixl 9 |!!
infixl 9 ||!
infixl 9 ||!!

(!) :: Unbox a => Holor a -> Int -> a
{-# INLINE (!) #-}
{- ^ Indexing a holor with shape (n,1) (column-vector) or (1,n) (row-vector)

>>> v = vector [5,6,7]
>>> v!1
6.0
-}
(!) vec idx
    | not $ length shape == 2 && (product shape == len)
        = error $ "(!) : indexing with single Int only possible for column or row vectors"
    | not $ isValidIdx len idx
        = error $ "(!) : index out of range"
    | otherwise = values V.! idx
    where
        shape = hShape vec
        values = hValues vec
        len = V.length values

(!!) :: Unbox a => Holor a -> Int -> a
{-# INLINE (!!) #-}
-- ^ Unsafe version of (!) without checking for row or column vector and not checking bounds
(!!) vec idx = (hValues vec) V.! idx


(|!) :: Unbox a => Holor a -> [Int] -> a
{-# INLINE (|!) #-}
{- ^ Indexing a single entry of the holor

>>> m = matrix [[2,3,4],[5,6,3],[9,0,2]]
>>> m|![0,2]
4.0
-}
(|!) hlr mIdx
    | isValidMIdx (hShape hlr) mIdx = hlr|!!mIdx
    | otherwise = error $ "(|!) : multiindex " ++ show mIdx ++ " out of bounds"

(|!!) :: Unbox a => Holor a -> [Int] -> a
{-# INLINE (|!!) #-}
-- ^ Unsafe version of (|!) without checking bounds
(|!!) hlr mIdx = V.unsafeIndex (hValues hlr) vectorindex where
    vectorindex = fromMultiIndexToIndex (hStrides hlr) mIdx



(||!) :: Unbox a => Holor a -> [[Int]] -> Holor a
{-# INLINE (||!) #-}
{- ^ Multidimensional indexing (extract a subholor). For each dimension a list of indices define the axes to extract. This means multiindexing is done by a nested list of indices which has the same length as the shape. The dimension of the extracted subholor remains the same as that of the original holor.

>>> h = holor [4,4] [1..16]
>>> h
  1.0  2.0  3.0  4.0
  5.0  6.0  7.0  8.0
  9.0  10.0  11.0  12.0
  13.0  14.0  15.0  16.0
>>> h||![[1,2,3],[0,1,1,2,3]]
  5.0  6.0  6.0  7.0  8.0
  9.0  10.0  10.0  11.0  12.0
  13.0  14.0  14.0  15.0  16.0
-}
(||!) hlr mIdcs
    | isValidMIdcs (hShape hlr) mIdcs = hlr||!!mIdcs
    | otherwise = error $ "(||!) : multiindices " ++ show mIdcs ++ " invalid"

(||!!) :: Unbox a => Holor a -> [[Int]] -> Holor a
{-# INLINE (||!!) #-}
-- ^ Unsafe version of (||!) without checking multiindices
(||!!) hlr mIdcs = Holor shp (frohShapeToStrides shp) (V.fromList values) where
    shp = fmap length mIdcs
    values = [ hlr|!!indices | indices <- cartProd mIdcs]


{- TODO:

slice = undefined

unsafeSlice = undefined

Idea : use V.unsafeSlice and V.thaw

-}

----------------
-- ** Operations

map :: (Unbox a, Unbox b) => (a -> b) -> Holor a -> Holor b
{-# INLINE map #-}
-- ^ map a function over all entries of a holor
map f hlr = Holor (hShape hlr) (hStrides hlr) (V.map f $ hValues hlr)


filter :: (Unbox a) => (a -> Bool) -> Holor a -> [a]
{-# INLINE filter #-}
{- ^ filter the values of a holor by a condition. The remaining elements are returned in a list.

>>> h = holor [3,3,3] [1 .. 27] :: Holor Int
>>> filter (\e -> mod e 3 == 0) h
[3,6,9,12,15,18,21,24,27]
-}
filter cond hlr = V.toList $ V.filter cond (hValues hlr)

-- TODO: may be working also with V.findIndices!
selectBy :: Unbox a => (a -> Bool) -> Holor a -> [[Int]]
{-# INLINE selectBy #-}
{- ^ select all multiindices of a holor whose corresponding elements fulfill a condition.

>>> m = matrix [[-1,1],[-2,3]]
>>> selectBy (>0) m
  [[0,1],[1,1]]
-}
selectBy cond hlr = fillList 0 [] where
    fillList idx list
        | idx < V.length (hValues hlr)
            = case () of
              _ | cond val -> (fromIndexToMultiIndex (hStrides hlr) idx) : (fillList (idx+1) list)
                | otherwise -> fillList (idx+1) list
        | otherwise = list
        where val = (hValues hlr)V.!idx



countBy :: Unbox a => (a -> Bool) -> Holor a -> Int
{-# INLINE countBy #-}
-- ^ count elements of a holor that fulfill a condition
countBy cond hlr = V.sum $ V.map fromEnum (V.map cond (hValues hlr))

accumulate :: Unbox a => (a -> a -> a) -> Holor a -> [a]
{-# INLINE accumulate #-}
{- ^ accumulate the values of a holor by a function.

>>> v = vector [2,3,4,1] :: Holor Int
>>> accumulate (+) v
  [2,5,9,10]
>>> accumulate max v
  [2,3,4,4]
-}
accumulate f hlr = V.toList $ V.scanl1 f (hValues hlr)

flatten :: Unbox a => Holor a -> Holor a
{-# INLINE flatten #-}
-- ^ flatten a holor to a column vector
flatten hlr = Holor [product $ hShape hlr, 1] [1,1] (hValues hlr)

transpose :: Unbox a => Holor a -> Holor a
{-# INLINE transpose #-}
-- ^ transpose a holor
transpose hlr = Holor shapeOut stridesOut valuesOut where
    shapeOut = reverse (hShape hlr)
    stridesOut = frohShapeToStrides shapeOut
    valuesOut = V.fromList [ hlr|!!(reverse idcs) | idcs <- frohShapeToMultiIndices shapeOut]

{-
-- TODO:
-- Idea: Use V.unsafeSlice. Should be easier than flipCols since values are in row-major order.
flipRows :: Int -> Holor a -> Holor a
{-# INLINE flipRows #-}
-- ^ flip rows in reversed order for a given dimension
flipRows dim hlr = undefined
    -- = Holor (shape hlr) (hStrides hlr) values where
    -- values = undefined

-- TODO:
-- Idea: first transpose holor, then use flipRows, then transpose again?
flipCols :: Int -> Holor a -> Holor a
{-# INLINE flipCols #-}
-- ^ flip columns in reversed order for a given dimension
flipCols dim hlr = undefined
-}

{-
-- TODO:
tile = undefined

-- TODO:
concat = undefined
-}

reshape :: Unbox a => Holor a -> [Int] -> Holor a
{-# INLINE reshape #-}
{- ^ reshape a holor without changing the order of the underlying values

>>> h = holor [2,2,2] [1 .. 8]
>>> reshape h [2,4]
  1.0  2.0  3.0  4.0
  5.0  6.0  7.0  8.0
-}
reshape hlr shp
    | product (shape hlr) == product shp = Holor shp (frohShapeToStrides shp) (hValues hlr)
    | otherwise = error $
        "reshape : cannot reshape from " ++ show (shape hlr) ++ " to " ++ show shp


diagonal :: Unbox a => Holor a -> Holor a
{-# INLINE diagonal #-}
-- ^ get diagonal of a quadratical matrix (2d-holor) as a column vector
diagonal hlr
    | isSquare hlr = vector [hlr |!! [i,i] | i <- [0 .. head (shape hlr)-1]]
    | otherwise = error $ "diagonal : Holor must be in quadratical matrix form"



-------------------
-- ** Conversions

toList :: Unbox a => Holor a -> [a]
{-# INLINE toList #-}
-- ^ returns the holor values in a list
toList hlr = V.toList $ hValues hlr

toList2 :: Unbox a => Holor a -> [[a]]
{-# INLINE toList2 #-}
-- ^ returns the values of an 2d-holor in a list of lists
toList2 hlr
    | not (length shp == 2) = error $ "toList2 : not a 2d-holor"
    | otherwise = [ [ hlr|!![i,j] | j <- [0 .. n-1]] | i <- [0 .. m-1]]
    where
        shp = shape hlr
        [m,n] = shp

toScalar :: Unbox a => Holor a -> a
{-# INLINE toScalar #-}
-- ^ converts a holor with a single element to the element itself
toScalar hlr
    | V.length (hValues hlr) == 1 = V.head (hValues hlr)
    | otherwise = error $ "toScalar : not a holor with a single element"


toT2 :: Unbox a => Holor a -> T2 a
-- ^ converts a holor to a 2-tuple if possible
toT2 hlr
    | hShape hlr == [2,1] || hShape hlr == [1,2] = (t1,t2)
    | otherwise = error $ "toT2 : shape mismatch"
    where
        [t1,t2] = toList hlr

toT3 :: Unbox a => Holor a -> T3 a
-- ^ converts a holor to a 3-tuple if possible
toT3 hlr
    | hShape hlr == [3,1] || hShape hlr == [1,3] = (t1,t2,t3)
    | otherwise = error $ "toT3 : shape mismatch"
    where
        [t1,t2,t3] = toList hlr

toT4 :: Unbox a => Holor a -> T4 a
-- ^ converts a holor to a 4-tuple if possible
toT4 hlr
    | hShape hlr == [4,1] || hShape hlr == [1,4] = (t1,t2,t3,t4)
    | otherwise = error $ "toT4 : shape mismatch"
    where
        [t1,t2,t3,t4] = toList hlr

toT22 :: Unbox a => Holor a -> T22 a
-- ^ converts a holor to nested 2,2-tuple if possible
toT22 hlr
    | hShape hlr == [2,2] = ((t11,t12),(t21,t22))
    | otherwise = error $ "toT22 : shape mismatch"
    where
        [t11,t12,t21,t22] = toList hlr

toT32 :: Unbox a => Holor a -> T32 a
-- ^ converts a holor to nested 3,2-tuple if possible
toT32 hlr
    | hShape hlr == [3,2] = ((t11,t12),(t21,t22),(t31,t32))
    | otherwise = error $ "toT32 : shape mismatch"
    where
        [t11,t12,t21,t22,t31,t32] = toList hlr

toT42 :: Unbox a => Holor a -> T42 a
-- ^ converts a holor to nested 4,2-tuple if possible
toT42 hlr
    | hShape hlr == [4,2] = ((t11,t12),(t21,t22),(t31,t32),(t41,t42))
    | otherwise = error $ "toT42 : shape mismatch"
    where
        [t11,t12,t21,t22,t31,t32,t41,t42] = toList hlr

toT23 :: Unbox a => Holor a -> T23 a
-- ^ converts a holor to nested 2,3-tuple if possible
toT23 hlr
    | hShape hlr == [2,3] = ((t11,t12,t13),(t21,t22,t23))
    | otherwise = error $ "toT23 : shape mismatch"
    where
        [t11,t12,t13,t21,t22,t23] = toList hlr

toT33 :: Unbox a => Holor a -> T33 a
-- ^ converts a holor to nested 3,3-tuple if possible
toT33 hlr
    | hShape hlr == [3,3] = ((t11,t12,t13),(t21,t22,t23),(t31,t32,t33))
    | otherwise = error $ "toT33 : shape mismatch"
    where
        [t11,t12,t13,t21,t22,t23,t31,t32,t33] = toList hlr

toT43 :: Unbox a => Holor a -> T43 a
-- ^ converts a holor to nested 4,3-tuple if possible
toT43 hlr
    | hShape hlr == [4,3] = ((t11,t12,t13),(t21,t22,t23),(t31,t32,t33),(t41,t42,t43))
    | otherwise = error $ "toT43 : shape mismatch"
    where
        [t11,t12,t13,t21,t22,t23,t31,t32,t33,t41,t42,t43] = toList hlr

toT24 :: Unbox a => Holor a -> T24 a
-- ^ converts a holor to nested 2,4-tuple if possible
toT24 hlr
    | hShape hlr == [2,4] = ((t11,t12,t13,t14),(t21,t22,t23,t24))
    | otherwise = error $ "toT24 : shape mismatch"
    where
        [t11,t12,t13,t14,t21,t22,t23,t24] = toList hlr

toT34 :: Unbox a => Holor a -> T34 a
-- ^ converts a holor to nested 3,4-tuple if possible
toT34 hlr
    | hShape hlr == [3,4] = ((t11,t12,t13,t14),(t21,t22,t23,t24),(t31,t32,t33,t34))
    | otherwise = error $ "toT34 : shape mismatch"
    where
        [t11,t12,t13,t14,t21,t22,t23,t24,t31,t32,t33,t34] = toList hlr

toT44 :: Unbox a => Holor a -> T44 a
-- ^ converts a holor to nested 4,4-tuple if possible
toT44 hlr
    | hShape hlr ==[4,4] =((t11,t12,t13,t14),(t21,t22,t23,t24),(t31,t32,t33,t34),(t41,t42,t43,t44))
    | otherwise = error $ "toT44 : shape mismatch"
    where
        [t11,t12,t13,t14,t21,t22,t23,t24,t31,t32,t33,t34,t41,t42,t43,t44] = toList hlr


-------------------
-- ** Modifications

setElem :: Unbox a => Holor a -> [Int] -> a -> Holor a
{-# INLINE setElem #-}
{- ^ modify a single entry of a holor

>>> v = vector [1,2,3]
>>> setElem v [1] 22
  1.0  22.0  3.0
-}
setElem hlr mIdx value
    | not (isValidMIdx (shape hlr) mIdx) = error $ "setElem : multiindex invalid"
    | otherwise = Holor (hShape hlr) (hStrides hlr) valuesOut
    where
    valuesOut = V.create $ do
        -- first yields mutable copy, then modifies and freezes it:
        vec <- V.thaw $ hValues hlr
        let idx = fromMultiIndexToIndex (hStrides hlr) mIdx
        VM.unsafeWrite vec idx value
        return vec


setElems :: Unbox a => Holor a -> [[Int]] -> [a] -> Holor a
{-# INLINE setElems #-}
{- ^ modify many elements of a holor at once

>>> h = zeros [3,3] :: Holor Int
>>> setElems h [[0,0],[1,1],[2,2]] [4,5,6]
  4  0  0
  0  5  0
  0  0  6
-}
-- TODO: Is there an faster alternative with V.(//) or V.unsafeUpd ?
setElems hlr mIdxList values
    | length mIdxList /= length values =
        error $ "setElems : length of list with multiindexes unequal to length of values"
    | not (and (fmap (isValidMIdx (shape hlr)) mIdxList)) =
        error $ "setElems : list of multiindexes invalid"
    | otherwise = Holor (hShape hlr) (hStrides hlr) valuesOut
    where
    valuesOut = V.create $ do
        -- first yields mutable copy, then modifies and freezes it:
        vec <- V.thaw $ hValues hlr
        let idcs = fmap (fromMultiIndexToIndex (hStrides hlr)) mIdxList
        zipWithM_ (VM.unsafeWrite vec) idcs values
        return vec


-------------------
-- ** Checks

isValidHolor :: Unbox a => Holor a -> Bool
{-# INLINE isValidHolor #-}
-- ^ tests whether a holor is instanced correctly, i.e. shape, strides and values fit together
isValidHolor holor = V.length (hValues holor) == product (hShape holor)
    && frohShapeToStrides (hShape holor) == hStrides holor

isVector :: Unbox a => Holor a -> Bool
{-# INLINE isVector #-}
-- ^ tests whether a holor has shape [n,1] (column-vector) or [1,n] (row-vector)
isVector hlr =
    (length shp == 2) && (product shp == V.length (hValues hlr)) && (hStrides hlr == [1,1])
    where
        shp = hShape hlr

isSquare :: Unbox a => Holor a -> Bool
{-# INLINE isSquare #-}
-- ^ checks if a holor has the shape of a square matrix
isSquare hlr = (length shp == 2) && (and $ P.map (== head shp) (tail shp)) where
    shp = shape hlr
