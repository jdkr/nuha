{-# OPTIONS_HADDOCK hide, ignore-exports #-}
-- {-# LANGUAGE BangPatterns #-} -- for Debug.Trace

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Nuha.Numeric where

import qualified Prelude as P
import Prelude hiding (map, replicate)
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
-- import qualified Debug.Trace as D

import Math.Nuha.Types
import Math.Nuha.Base
import Math.Nuha.Internal

----------------
-- ** Constructions

diag :: (Unbox a, Num a) => Vector a -> Array a
{-# INLINE diag #-}
-- ^ diagonalizes a vector as 2d-array
diag vec = setValues arr0 mIdcs (V.toList vec) where
    len = V.length vec
    shape = [len, len]
    arr0 = zeros [len, len]
    mIdcs = [[i,i] | i <- [0..len-1]]

zeros :: (Unbox a, Num a) => [Int] -> Array a
{-# INLINE zeros #-}
-- ^ array with all entries zero
zeros shape = replicate shape 0

ones :: (Unbox a, Num a) => [Int] -> Array a
{-# INLINE ones #-}
-- ^ array with all entries one
ones shape = replicate shape 1

eye :: (Unbox a, Num a) => Int -> Array a
{-# INLINE eye #-}
-- ^ identity array
eye dim = diag $ V.replicate dim 1

linSpace :: (Unbox a, Fractional a, Enum a) => a -> a -> Int -> Vector a
{-# INLINE linSpace #-}
{- ^ creates a vector with points in linear sequence

>>> linSpace 1 3 5
[1.0,1.5,2.0,2.5,3.0]
-}
linSpace start stop num = V.fromList values where
    values = [start + i*step | i <- [0.0 .. fromIntegral(num-1)]]
    step = (stop - start)/(fromIntegral (num-1))


---------------
-- ** Operators

infixl 7 *|
infixr 7 |*
infixl 6 |+|
infixl 6 |-|
infixl 7 |*|
infixl 7 |.|
infixl 7 |.\
infixr 8 |**
infixr 8 |^
infixl 7 /.\

(*|) :: (Unbox a, Num a) => a -> Array a -> Array a
{-# INLINE (*|) #-}
-- ^ Array multiplied by a scalar from left
(*|) factor arr = map (*factor) arr

(|*) :: (Unbox a, Num a) => Array a -> a -> Array a
{-# INLINE (|*) #-}
-- ^ Array multiplied by a scalar from right
(|*) arr factor = factor *| arr

(|+|) :: (Unbox a, Num a) => Array a -> Array a -> Array a
{-# INLINE (|+|) #-}
-- ^ elementwise array addition
(|+|) a1 a2
    | (aShape a1) == (aShape a2) =
        Array (aShape a1) (aStrides a1) (V.zipWith (+) (aValues a1) (aValues a2))
    | otherwise = error $ "(|+|) : Shape missmatch at elementwise array addition"


(|-|) :: (Unbox a, Num a) => Array a -> Array a -> Array a
{-# INLINE (|-|) #-}
-- ^ elementwise array subtraction
(|-|) a1 a2
    | (aShape a1) == (aShape a2) =
        Array (aShape a1) (aStrides a1) (V.zipWith (-) (aValues a1) (aValues a2))
    | otherwise = error $ "(|-|) : Shape missmatch at elementwise array subtraction"

(|*|) :: (Unbox a, Num a) => Array a -> Array a -> Array a
{-# INLINE (|*|) #-}
-- ^ elementwise array multiplication
(|*|) a1 a2
    | (aShape a1) == (aShape a2) =
        Array (aShape a1) (aStrides a1) (V.zipWith (*) (aValues a1) (aValues a2))
    | otherwise = error $ "(|*|) : Shape missmatch at elementwise array multiplication"


(|.|) :: (Unbox a, Num a) => Array a -> Array a -> Array a
{-# INLINE (|.|) #-}
-- ^ Array multiplication, last dimension of left array has to match first dimension of right array
(|.|) a1 a2
    | dim a1 < 2 || dim a2 < 2 = error $ "(|.|) : array dimensions have to be at least 2"
    | last (shape a1) /= matchdim =
        error $ "(|.|) : dimension mismatch: " ++ show (shape a1) ++ " , " ++ show (shape a2)
    | otherwise = Array shapeOut stridesOut valuesOut
    where
        shape1 = take (length (shape a1) - 1) (shape a1)
        -- !shape1_ = D.trace ("shape1: " ++ show shape1) ()
        shape2 = drop 1 (shape a2)
        -- !shape2_ = D.trace ("shape2: " ++ show shape2) ()
        shapeOut = shape1 ++ shape2
        stridesOut = fromShapeToStrides shapeOut
        matchdim = head (shape a2) -- also step
        -- !matchdim_ = D.trace ("matchdim: " ++ show matchdim) ()
        a2T = transpose a2
        valuesOut = V.fromList [
            let
                slice1 = V.unsafeSlice (i*matchdim) matchdim (aValues a1)
                slice2 = V.unsafeSlice (j*matchdim) matchdim (aValues a2T)
            in
                V.sum (V.zipWith (*) slice1 slice2)
            | i <- [0 .. (product shape1 - 1)], j <- [0 .. (product shape2 - 1)]]


(|.\) :: (Unbox a, Num a) => Array a -> Vector a -> Vector a
{-# INLINE (|.\) #-}
-- ^ Multiplication of an array and a vector with result as a vector
(|.\) arr vec
    | (length (shape arr) == 2) && (last (shape arr) == V.length vec) = values
    | otherwise =
        error $ "(|.\\) : dimension mismatch: " ++ show (shape arr) ++ " , " ++ show (V.length vec)
    where
        matchdim = V.length vec
        values = V.fromList [
            let
                slice = V.unsafeSlice (i*matchdim) matchdim (aValues arr)
            in
                V.sum (V.zipWith (*) slice vec)
            | i <- [0 .. matchdim-1]]


(|**) :: (Unbox a, Floating a) => Array a -> a -> Array a
{-# INLINE (|**) #-}
-- ^ Array to a power elementwise
(|**) arr p = map (**p) arr


-- TODO: may be optimized by saving power of two products
(|^) :: (Unbox a, Num a) => Array a -> Int -> Array a
{-# INLINE (|^) #-}
-- ^ Array multiplied with itself elementwise n-times
(|^) arr num
    | num<0 = error $ "(|^) : number of multiplications must be non negative"
    | not (isSquare arr) = error $ "(|^) : array is not of square shape :" ++ show (aShape arr)
    | num == 0 = eye $ (shape arr)!!0
    | otherwise = iterMult arr 0
    where
        iterMult arr_ n
            | n == 1 = iterMult arr (n+1)
            | n <= num = iterMult (arr |.| arr_) (n+1)
            | otherwise = arr_



(/.\) :: (Unbox a, Num a) => Vector a -> Vector a -> a
{-# INLINE (/.\) #-}
-- ^ Dot product for vectors
(/.\) v1 v2
    | V.length v1 == V.length v2 = V.sum $ V.zipWith (*) v1 v2
    | otherwise = error $ "(/.\\) : vectors of different lengths: "++
        show (V.length v1) ++ " and " ++ show (V.length v2)

dot :: (Unbox a, Num a) => Array a -> Array a -> a
{-# INLINE dot #-}
-- ^ dot product for 1d arrays
dot a1 a2
    | (length (shape a1) == 1) && (shape a1 == shape a2) =
        V.sum $ V.zipWith (*) (aValues a1) (aValues a2)
    | otherwise =
        error $ "dot : shapes " ++ show (shape a1) ++ " and " ++ show (shape a2) ++ "incompatible"


----------------
-- ** Operations

negate :: (Unbox a, Num a) => Array a -> Array a
{-# INLINE negate #-}
-- ^ additive inverse of an array
negate = map P.negate

abs :: (Unbox a, Num a) => Array a -> Array a
{-# INLINE abs #-}
-- ^ absolute values ​​by array elements
abs= map P.abs

signum :: (Unbox a, Num a) => Array a -> Array a
{-# INLINE signum #-}
-- ^ signum by array elements
signum = map P.signum


maximum :: (Unbox a, Ord a) => Array a -> a
{-# INLINE maximum #-}
-- ^ maximum of all array elements
maximum arr = V.maximum $ aValues arr

minimum :: (Unbox a, Ord a) => Array a -> a
{-# INLINE minimum #-}
-- ^ minimum of all array elements
minimum arr = V.minimum $ aValues arr

sum :: (Unbox a, Num a) => Array a -> a
{-# INLINE sum #-}
-- ^ sum all element of an array
sum arr = V.sum $ aValues arr

trace :: (Unbox a, Num a) => Array a -> a
{-# INLINE trace #-}
-- ^ trace of a matrix
trace arr
    | isSquare arr = V.sum $ diagonal arr
    | otherwise = error $
        "trace : array with shape " ++ show (shape arr) ++ " is not a square matrix"


norm :: (Unbox a, Real a, Unbox b, Floating b) => a -> Array a -> b
{-# INLINE norm #-}
-- ^ p-norm
norm p arr = (V.sum $ V.map (\v -> (realToFrac v)**p') (aValues arr))**(1/p') where
    p' = realToFrac p

norm1 :: (Unbox a, Num a) => Array a -> a
{-# INLINE norm1 #-}
-- ^ manhattan norm (p = 1)
norm1 arr = V.sum $ V.map (\v -> P.abs v) (aValues arr)

norm2 :: (Unbox a, Real a, Unbox b, Floating b) => Array a -> b
{-# INLINE norm2 #-}
-- ^ frobenius norm, equal to euclidian norm for 1d-array (p = 2)
norm2 arr = sqrt $ V.sum $ V.map (\v -> (realToFrac v)**2) (aValues arr)

normM :: (Unbox a, Num a, Ord a) => Array a -> a
{-# INLINE normM #-}
-- ^ maximum norm (p = inf)
normM arr = V.maximum $ V.map (\v -> P.abs v) (aValues arr)


{- TODO:
-- Link: https://en.wikipedia.org/wiki/Adjugate_matrix
adjugate :: (Unbox a, Num a) => Array a -> Array a
{-# INLINE adjugate #-}
-- ^ adjugate Matrix
adjugate arr = undefined
-}

adjugate22 :: (Unbox a, Num a) => Array a -> Array a
{-# INLINE adjugate22 #-}
-- ^ adjugate of 2x2 matrix
adjugate22 arr22
    | shape arr22 == [2,2] = array [2,2] values
    | otherwise = error ("adjugate22 only possible for array with shape [2,2]")
    where
        m00 =   arr22||![0,0]
        m01 = - arr22||![1,0]
        m10 = - arr22||![0,1]
        m11 =   arr22||![1,1]
        values = [m11, m10, m01, m00]

adjugate33 :: (Unbox a, Num a) => Array a -> Array a
{-# INLINE adjugate33 #-}
-- ^ adjugate of 3x3 matrix
adjugate33 arr33
    | shape arr33 == [3,3] = array [3,3] values
    | otherwise = error ("adjugate33 only possible for array with shape [3,3]")
    where
        a00 = arr33||![0,0]; a01 = arr33||![0,1]; a02 = arr33||![0,2]
        a10 = arr33||![1,0]; a11 = arr33||![1,1]; a12 = arr33||![1,2]
        a20 = arr33||![2,0]; a21 = arr33||![2,1]; a22 = arr33||![2,2]
        m00 = a11*a22 - a12*a21
        m01 = a12*a20 - a10*a22
        m02 = a10*a21 - a11*a20
        m10 = a02*a21 - a01*a22
        m11 = a00*a22 - a02*a20
        m12 = a01*a20 - a00*a21
        m20 = a01*a12 - a02*a11
        m21 = a02*a10 - a00*a12
        m22 = a00*a11 - a01*a10
        values = [m00, m10, m20, m01, m11, m21, m02, m12, m22]

-- Link: https://semath.info/src/inverse-cofactor-ex4.html
-- (Adj44)_ij = (-1)^(i+j) * det33(M_ji)
adjugate44 :: (Unbox a, Num a) => Array a -> Array a
{-# INLINE adjugate44 #-}
-- ^ adjugate of 4x4 matrix
adjugate44 arr44
    | shape arr44 == [4,4] = array [4,4] values
    | otherwise = error ("adjugate44 only possible for array with shape [4,4]")
    where
        a00 = arr44||![0,0]; a01 = arr44||![0,1]; a02 = arr44||![0,2]; a03 = arr44||![0,3]
        a10 = arr44||![1,0]; a11 = arr44||![1,1]; a12 = arr44||![1,2]; a13 = arr44||![1,3]
        a20 = arr44||![2,0]; a21 = arr44||![2,1]; a22 = arr44||![2,2]; a23 = arr44||![2,3]
        a30 = arr44||![3,0]; a31 = arr44||![3,1]; a32 = arr44||![3,2]; a33 = arr44||![3,3]
        m00 =   (det33 $ array [3,3] [a11,a12,a13,a21,a22,a23,a31,a32,a33])
        m01 = - (det33 $ array [3,3] [a10,a12,a13,a20,a22,a23,a30,a32,a33])
        m02 =   (det33 $ array [3,3] [a10,a11,a13,a20,a21,a23,a30,a31,a33])
        m03 = - (det33 $ array [3,3] [a10,a11,a12,a20,a21,a22,a30,a31,a32])
        m10 = - (det33 $ array [3,3] [a01,a02,a03,a21,a22,a23,a31,a32,a33])
        m11 =   (det33 $ array [3,3] [a00,a02,a03,a20,a22,a23,a30,a32,a33])
        m12 = - (det33 $ array [3,3] [a00,a01,a03,a20,a21,a23,a30,a31,a33])
        m13 =   (det33 $ array [3,3] [a00,a01,a02,a20,a21,a22,a30,a31,a32])
        m20 =   (det33 $ array [3,3] [a01,a02,a03,a11,a12,a13,a31,a32,a33])
        m21 = - (det33 $ array [3,3] [a00,a02,a03,a10,a12,a13,a30,a32,a33])
        m22 =   (det33 $ array [3,3] [a00,a01,a03,a10,a11,a13,a30,a31,a33])
        m23 = - (det33 $ array [3,3] [a00,a01,a02,a10,a11,a12,a30,a31,a32])
        m30 = - (det33 $ array [3,3] [a01,a02,a03,a11,a12,a13,a21,a22,a23])
        m31 =   (det33 $ array [3,3] [a00,a02,a03,a10,a12,a13,a20,a22,a23])
        m32 = - (det33 $ array [3,3] [a00,a01,a03,a10,a11,a13,a20,a21,a23])
        m33 =   (det33 $ array [3,3] [a00,a01,a02,a10,a11,a12,a20,a21,a22])
        values = [m00,m10,m20,m30,m01,m11,m21,m31,m02,m12,m22,m32,m03,m13,m23,m33]

{- TODO:
det :: (Unbox a, Num a) => Array a -> a
{-# INLINE det #-}
det arr
    | (length (shape arr) == 2) && ((shape arr)!!0 == (shape arr)!!1) = result
    | otherwise = error ("det of nonsquare array with shape " ++ show (shape arr))
    where
        result = undefined
-}


det22 :: (Unbox a, Num a) => Array a -> a
{-# INLINE det22 #-}
-- ^ determinant of 2x2 matrix
det22 arr22
    | shape arr22 == [2,2] = a00*a11 - a01*a10
    | otherwise = error ("det22 only possible for array with shape [2,2]")
    where
        a00 = arr22||![0,0]
        a01 = arr22||![0,1]
        a10 = arr22||![1,0]
        a11 = arr22||![1,1]

-- Laplace-Expansion by first row
det33 :: (Unbox a, Num a) => Array a -> a
{-# INLINE det33 #-}
-- ^ determinant of 3x3 matrix
det33 arr33
    | shape arr33 == [3,3] = a00*(det22 m00) - a01*(det22 m01) + a02*(det22 m02)
    | otherwise = error ("det33 only possible for array with shape [3,3]")
    where
        a00 = arr33||![0,0]
        a01 = arr33||![0,1]
        a02 = arr33||![0,2]
        m00 = arr33||!![[1,2],[1,2]]
        m01 = arr33||!![[1,2],[0,2]]
        m02 = arr33||!![[1,2],[0,1]]

-- Laplace-Expansion by first row
det44 :: (Unbox a, Num a) => Array a -> a
{-# INLINE det44 #-}
-- ^ determinant of 4x4 matrix
det44 arr44
    | shape arr44 == [4,4] = a00*(det33 m00) - a01*(det33 m01) + a02*(det33 m02) - a03*(det33 m03)
    | otherwise = error ("det44 only possible for array with shape [4,4]")
    where
        a00 = arr44||![0,0]
        a01 = arr44||![0,1]
        a02 = arr44||![0,2]
        a03 = arr44||![0,3]
        m00 = arr44||!![[1,2,3],[1,2,3]]
        m01 = arr44||!![[1,2,3],[0,2,3]]
        m02 = arr44||!![[1,2,3],[0,1,3]]
        m03 = arr44||!![[1,2,3],[0,1,2]]


{- TODO:
inv :: (Unbox a, Fractional a) => Array a -> Array a
{-# INLINE inv #-}
inv arr = error "not implemented"
-}

-- Link: https://de.wikipedia.org/wiki/Inverse_Matrix#Explizite_Formeln
inv22 :: (Unbox a, Fractional a) => Array a -> Array a
{-# INLINE inv22 #-}
-- ^ inverse of 2x2 matrix
inv22 arr22
    | shape arr22 == [2,2] = (1/det22 arr22) *| (adjugate22 arr22)
    | otherwise = error ("inv22 only possible for array with shape [2,2]")

-- Link: https://de.wikipedia.org/wiki/Inverse_Matrix#Explizite_Formeln
inv33 :: (Unbox a, Fractional a) => Array a -> Array a
{-# INLINE inv33 #-}
-- ^ inverse of 3x3 matrix
inv33 arr33
    | shape arr33 == [3,3] = (1/det33 arr33) *| (adjugate33 arr33)
    | otherwise = error ("inv33 only possible for array with shape [3,3]")

-- Link: https://semath.info/src/inverse-cofactor-ex4.html
inv44 :: (Unbox a, Fractional a) => Array a -> Array a
{-# INLINE inv44 #-}
-- ^ inverse of 4x4 matrix
inv44 arr44
    | shape arr44 == [4,4] = (1/det44 arr44) *| (adjugate44 arr44)
    | otherwise = error ("inv44 only possible for array with shape [4,4]")


selectNonzero :: (Unbox a, Num a, Eq a) => Array a -> [[Int]]
{-# INLINE selectNonzero #-}
-- ^ select all indices of array with nonzero elements
selectNonzero arr = selectBy (/=0) arr

countNonzero :: (Unbox a, Num a, Eq a) => Array a -> Int
{-# INLINE countNonzero #-}
-- ^ Number of nonzero elements
countNonzero arr = countBy (/=0) arr



