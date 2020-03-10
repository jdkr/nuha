{-# OPTIONS_HADDOCK hide, ignore-exports #-}
-- {-# LANGUAGE BangPatterns #-}  -- for Debug.Trace

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Nuha.Base where

import qualified Prelude as P
import Prelude hiding (map, replicate)
import Control.Monad (zipWithM_)
-- import qualified Debug.Trace as D
import Foreign.Storable (Storable, sizeOf)
import Data.Vector.Unboxed (Vector, Unbox, (!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import Math.Nuha.Types
import Math.Nuha.Internal


-- Naming conventions for indices:
-- idx : index for aValues :: Int
-- idcs : list of indices for aValues :: [Int]
-- mIdx : multiindex for an array :: [Int]
-- mIdcs : list of multiindices for an array :: [[Int]]


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

array :: Unbox a => [Int] -> [a] -> Array a
{-# INLINE array #-}
{- ^ Basic function for creating a multidimensional array

>>> array [2,2] [1,2,3,4]
  1.0  2.0
  3.0  4.0
>>> array [2,2] [1,2,3,4] :: Array Int
  1  2
  3  4
>>> array [2,2,4] [1 .. 16]
[0,:,:] =
  1.0  2.0  3.0  4.0
  5.0  6.0  7.0  8.0
[1,:,:] =
  9.0  10.0  11.0  12.0
  13.0  14.0  15.0  16.0

-}
array shape values
    | foldr (*) 1 shape == length values =
        Array shape (fromShapeToStrides shape) (V.fromList values)
    | otherwise = error $ "array : length of values " ++ show (length values) ++
        " doesn't match to shape " ++ show shape

-- TODO: Fehlerbehandlung wenn values ungleich [a]
array1d :: Unbox a => [a] -> Array a
{-# INLINE array1d #-}
{- ^ Convenience function for creating a 1d-array

>>> array1d [1,2,3,4]
 1.0  2.0  3.0  4.0
-}
array1d values = Array shape (fromShapeToStrides shape) (V.fromList values) where
    shape = [length values]

array2d :: Unbox a => [[a]] -> Array a
{-# INLINE array2d #-}
{- ^ Convenience function for creating a 2d-array

>>> array2d [[1,2],[3,4]]
  1.0  2.0
  3.0  4.0
-}
array2d valuesList
    | lengths1dAreEqual = Array shape (fromShapeToStrides shape) (V.fromList values)
    | otherwise = error $ "array2d : shape mismatch in nested list"
    where
        lengths1d = [length v1 | v1 <- valuesList]
        lengths1dAreEqual = all (== head lengths1d) (tail lengths1d)
        values = [entry | row <- valuesList , entry <- row]
        shape = [length valuesList, head lengths1d]

fromVector :: Unbox a => [Int] -> Vector a -> Array a
{-# INLINE fromVector #-}
{- ^ Creates an array from a vector from 'Data.Vector.Unboxed'

>>> v = V.fromList [1,2,3,4]
>>> fromVector [2,2] v
  1.0  2.0
  3.0  4.0
-}
fromVector shp vec
    | foldr (*) 1 shp == V.length vec = Array shp (fromShapeToStrides shp) vec
    | otherwise = error $ "fromVector : length of vector " ++ show (V.length vec) ++
        " doesn't match to shape " ++ show shp


replicate :: Unbox a => [Int] -> a -> Array a
{-# INLINE replicate #-}
{- ^ Creates an array for a given shape with all entries the same

>>> replicate [3] 0
  0.0  0.0  0.0
-}
replicate shape value = array shape (P.replicate (product shape) value)


----------------
-- ** Properties

shape :: Unbox a => Array a -> [Int]
{-# INLINE shape #-}
-- ^ Shape of an array
shape = aShape

numItems :: Unbox a => Array a -> Int
{-# INLINE numItems #-}
-- ^ Number of entrys in array
numItems arr = product (shape arr)

sizeItems :: (Unbox a, Storable a) => Array a -> Int
{-# INLINE sizeItems #-}
-- ^ Size of all array elements in bytes (not equal to consumed memory of the array)
sizeItems arr = (numItems arr) * (sizeItem arr)

sizeItem :: (Unbox a, Storable a) => Array a -> Int
{-# INLINE sizeItem #-}
-- ^ Size of a single array element in bytes
sizeItem arr = sizeOf $ (aValues arr)!0

dim :: Unbox a => Array a -> Int
{-# INLINE dim #-}
{- ^ Dimension of an array (length of shape)

>>> dim $ array [2,2,2] [0..7]
3
-}
dim = length . aShape


---------------
-- ** Accessors

infixl 9 |!
infixl 9 ||!
infixl 9 |!!
infixl 9 ||!!


(|!) :: Unbox a => Array a -> [Int] -> a
{-# INLINE (|!) #-}
{- ^ Indexing a single entry of the array

>>> a = array2d [[2,3,4],[5,6,3],[9,0,2]]
>>> a|![0,2]
4.0
-}
(|!) arr mIdx
    | isValidMIdx (aShape arr) mIdx = arr||!mIdx
    | otherwise = error $ "(|!) : multiindex " ++ show mIdx ++ " out of bounds"

(||!) :: Unbox a => Array a -> [Int] -> a
{-# INLINE (||!) #-}
-- ^ Unsafe version of (|!) without checking bounds
(||!) arr mIdx = V.unsafeIndex (aValues arr) vectorindex where
    vectorindex = fromMultiIndexToIndex (aStrides arr) mIdx


-- TODO: Test1 OK für 2d-array. TODO: Teste noch höherdimensionale arrays
(|!!) :: Unbox a => Array a -> [[Int]] -> Array a
{-# INLINE (|!!) #-}
{- ^ Multidemensional indexing (extract a subarray). For each dimension a list of indices define the axes to extract. This means multiindexing is done by a nested list of indices which has the same length as the shape. The dimension of the extracted arrays stays the same as the original array.

>>> a = array [4,4] [1..16]
>>> a
  1.0  2.0  3.0  4.0
  5.0  6.0  7.0  8.0
  9.0  10.0  11.0  12.0
  13.0  14.0  15.0  16.0
>>> a|!![[1,2,3],[0,1,1,2,3]]
  5.0  6.0  6.0  7.0  8.0
  9.0  10.0  10.0  11.0  12.0
  13.0  14.0  14.0  15.0  16.0
-}
(|!!) arr mIdcs
    | isValidMIdcs (aShape arr) mIdcs = arr||!!mIdcs
    | otherwise = error $ "(|!!) : multiindices " ++ show mIdcs ++ " invalid"

(||!!) :: Unbox a => Array a -> [[Int]] -> Array a
{-# INLINE (||!!) #-}
-- ^ Unsafe version of (|!!) without checking multiindices
(||!!) arr mIdcs = Array shp (fromShapeToStrides shp) (V.fromList values) where
    shp = fmap length mIdcs
    values = [ arr||!indices | indices <- cartProd mIdcs]


{- TODO:
-- TODO: slice soll die Funktion V.unsafeSlice ausnutzen (auch hier zwei Versionen slice und unsafeSlice implementieren). TODO: Falls der Slice "quer" zur Richtung im Speicher verläuft erst ein transpose durchführen. FRAGE: Wieder mit V.thaw arbeiten?

slice = undefined

unsafeSlice = undefined
-}

----------------
-- ** Operations

map :: (Unbox a, Unbox b) => (a -> b) -> Array a -> Array b
{-# INLINE map #-}
-- ^ map a function over all entries of an array
map f arr = Array (aShape arr) (aStrides arr) (V.map f $ aValues arr)


filter :: (Unbox a) => (a -> Bool) -> Array a -> Vector a
{-# INLINE filter #-}
{- ^ filter the values of an array by a condition. The remaining elements are returned in a vector.

>>> a = array [3,3,3] [1 .. 27] :: Array Int
>>> filter (\e -> mod e 3 == 0) a
[3,6,9,12,15,18,21,24,27]
-}
filter cond arr = V.filter cond (aValues arr)

-- TODO: may be working also with V.findIndices!
selectBy :: Unbox a => (a -> Bool) -> Array a -> [[Int]]
{-# INLINE selectBy #-}
{- ^ select all indices of an array that fulfill a condition

>>> a = array2d [[-1,1],[-2,3]]
>>> selectBy (>0) a
  [[0,1],[1,1]]
-}
selectBy cond arr = fillList 0 [] where
    fillList idx list
        | idx < V.length (aValues arr)
            = case () of
              _ | cond val -> (fromIndexToMultiIndex (aStrides arr) idx) : (fillList (idx+1) list)
                | otherwise -> fillList (idx+1) list
        | otherwise = list
        where val = (aValues arr)!idx



countBy :: Unbox a => (a -> Bool) -> Array a -> Int
{-# INLINE countBy #-}
-- ^ count elements of array that fulfill a condition
countBy cond arr = V.sum $ V.map fromEnum (V.map cond (aValues arr))

accumulate :: Unbox a => (a -> a -> a) -> Array a -> Vector a
{-# INLINE accumulate #-}
{- ^ accumulate the values of an array by a function

>>> a = array1d [2,3,4,1] :: Array Int
>>> accumulate (+) a
  [2,5,9,10]
>>> accumulate max a
  [2,3,4,4]
-}
accumulate f arr = V.scanl1 f (aValues arr)


flatten :: Unbox a => Array a -> Array a
{-# INLINE flatten #-}
-- ^ flatten an array to a 1d-array
flatten arr = Array [product $ aShape arr] [1] (aValues arr)

toVector :: Unbox a => Array a -> Vector a
{-# INLINE toVector #-}
-- ^ flatten an array to a vector
toVector arr = aValues arr

transpose :: Unbox a => Array a -> Array a
{-# INLINE transpose #-}
-- ^ transpose an array
transpose arr = Array shapeOut stridesOut valuesOut where
    shapeOut = reverse (shape arr)
    stridesOut = fromShapeToStrides shapeOut
    valuesOut = V.fromList [ arr||!(reverse idcs) | idcs <- fromShapeToMultiIndices shapeOut]

{-
-- TODO:
-- Idea: Use V.unsafeSlice. Should be easier than flipCols since values are in row-major order.
flipRows :: Int -> Array a -> Array a
{-# INLINE flipRows #-}
-- ^ flip rows in reversed order for a given dimension
flipRows dim arr = undefined
    -- = Array (shape arr) (aStrides arr) values where
    -- values = undefined

-- TODO:
-- Idea: first transpose array, then use flipRows, then transpose again?
flipCols :: Int -> Array a -> Array a
{-# INLINE flipCols #-}
-- ^ flip columns in reversed order for a given dimension
flipCols dim arr = undefined
-}

{-
-- TODO:
tile = undefined

-- TODO:
concat = undefined
-}


reshape :: Unbox a => Array a -> [Int] -> Array a
{-# INLINE reshape #-}
{- ^ reshape an array without changing the order of underlying values

>>> a = array [2,2,2] [1 .. 8]
>>> reshape a [2,4]
  1.0  2.0  3.0  4.0
  5.0  6.0  7.0  8.0
-}
reshape arr shp
    | product (shape arr) == product shp = Array shp (fromShapeToStrides shp) (aValues arr)
    | otherwise = error $
        "reshape : cannot reshape from " ++ show (shape arr) ++ " to " ++ show shp


diagonal :: Unbox a => Array a -> Vector a
{-# INLINE diagonal #-}
-- ^ get diagonal of a quadratical matrix (2d-array) as a vector
diagonal arr
    | isSquare arr = V.fromList [arr ||! [i,i] | i <- [0 .. head (shape arr)-1]]
    | otherwise = error $ "diagonal : Array must me 2d and quadratical"


isSquare :: Unbox a => Array a -> Bool
{-# INLINE isSquare #-}
-- ^ checks if an array has the shape of a square matrix
isSquare arr = (length shp == 2) && (and $ P.map (== head shp) (tail shp)) where
    shp = shape arr



-------------------
-- ** Modifications


setValue :: Unbox a => Array a -> [Int] -> a -> Array a
{-# INLINE setValue #-}
{- ^ modify a single entry of an array

>>> a = array1d [1,2,3]
>>> setValue a [1] 22
  1.0  22.0  3.0
-}
setValue arr mIdx value
    | not (isValidMIdx (shape arr) mIdx) = error $ "setValue : multiindex invalid"
    | otherwise = Array (aShape arr) (aStrides arr) valuesOut
    where
    valuesOut = V.create $ do
        -- first yields mutable copy, then modifies and freezes it:
        vec <- V.thaw $ aValues arr
        let idx = fromMultiIndexToIndex (aStrides arr) mIdx
        VM.unsafeWrite vec idx value
        return vec


setValues :: Unbox a => Array a -> [[Int]] -> [a] -> Array a
{-# INLINE setValues #-}
{- ^ modify many elements of an array at once

>>> a = zeros [3,3] :: Array Int
>>> setValues a [[0,0],[1,1],[2,2]] [4,5,6]
  4  0  0
  0  5  0
  0  0  6
-}
setValues arr mIdxList values
    | length mIdxList /= length values =
        error $ "setValues : length of list with multiindexes unequal to length of values"
    | not (and (fmap (isValidMIdx (shape arr)) mIdxList)) =
        error $ "setValues : list of multiindexes invalid"
    | otherwise = Array (aShape arr) (aStrides arr) valuesOut
    where
    valuesOut = V.create $ do
        -- first yields mutable copy, then modifies and freezes it:
        vec <- V.thaw $ aValues arr
        let idcs = fmap (fromMultiIndexToIndex (aStrides arr)) mIdxList
        zipWithM_ (VM.unsafeWrite vec) idcs values
        return vec

