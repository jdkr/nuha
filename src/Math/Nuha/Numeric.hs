-- {-# LANGUAGE BangPatterns #-} -- for Debug.Trace

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Nuha.Numeric where

import qualified Prelude as P
import Prelude hiding (map, sum, replicate)
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V
-- import qualified Debug.Trace as D

import Math.Nuha.Types
import Math.Nuha.Base
import Math.Nuha.Internal


{- TODO:
Is there room for optimize things?. See:
    Paper: "Exploiting Vector Instructions with Generalized Stream Fusion"
    https://gitlab.haskell.org/ghc/ghc/-/wikis/simd
    https://gitlab.haskell.org/ghc/ghc/-/wikis/simd/design
    https://github.com/haskell/vector/issues/251
    https://github.com/Magalame/lascive
-}

----------------
-- ** Constructions

-- | diagonalizes a list of values as a square matrix
diag :: (Unbox a, Num a) => [a] -> Holor a
{-# INLINE diag #-}
diag lst = setElems mIdcs lst hlr0 where
    len = length lst
    shape = [len, len]
    hlr0 = zeros [len, len]
    mIdcs = [[i,i] | i <- [0..len-1]]

-- | holor with all entries zero
zeros :: (Unbox a, Num a) => [Int] -> Holor a
{-# INLINE zeros #-}
zeros shape = replicate shape 0

-- | holor with all entries one
ones :: (Unbox a, Num a) => [Int] -> Holor a
{-# INLINE ones #-}
ones shape = replicate shape 1

-- | identity matrix
eye :: (Unbox a, Num a) => Int -> Holor a
{-# INLINE eye #-}
eye dim = diag $ P.replicate dim 1

{- | creates a column vector with points in linear sequence

>>> linSpace 1 3 5
  1.0
  1.5
  2.0
  2.5
  3.0
-}
linSpace :: (Unbox a, Fractional a, Enum a) => a -> a -> Int -> Holor a
{-# INLINE linSpace #-}
linSpace start stop num = vector values where
    values = [start + i*step | i <- [0.0 .. fromIntegral(num-1)]]
    step = (stop - start)/(fromIntegral (num-1))


---------------
-- ** Operators

infixl 6 |+
infixl 6 +|
infixl 6 |-
infixl 6 -|
infixr 7 |*
infixl 7 *|
infixr 7 |/
infixr 7 /|
infixl 6 |+|
infixl 6 |-|
infixl 7 |*|
infixl 7 |.|
infixr 8 |**
infixr 8 |^

-- | Add a scalar from the right to each element of a holor
(|+) :: (Unbox a, Num a) => Holor a -> a -> Holor a
{-# INLINE (|+) #-}
(|+) hlr scalar = map (+scalar) hlr

-- | Add a scalar from the left to each element of a holor
(+|) :: (Unbox a, Num a) => a -> Holor a -> Holor a
{-# INLINE (+|) #-}
(+|) scalar hlr = map (+scalar) hlr

-- | Subtract a scalar from the right to each element of a holor
(|-) :: (Unbox a, Num a) => Holor a -> a -> Holor a
{-# INLINE (|-) #-}
(|-) hlr scalar = map (+(-scalar)) hlr

-- | A single scalar is subtracted by each element of a holor
(-|) :: (Unbox a, Num a) => a -> Holor a -> Holor a
{-# INLINE (-|) #-}
(-|) scalar hlr = map (scalar-) hlr

-- | Holor multiplied by a scalar from right
(|*) :: (Unbox a, Num a) => Holor a -> a -> Holor a
{-# INLINE (|*) #-}
(|*) hlr factor = map (*factor) hlr

-- | Holor multiplied by a scalar from left
(*|) :: (Unbox a, Num a) => a -> Holor a -> Holor a
{-# INLINE (*|) #-}
(*|) factor hlr = map (*factor) hlr

-- | Holor divided by a scalar
(|/) :: (Unbox a, Fractional a) => Holor a -> a -> Holor a
{-# INLINE (|/) #-}
(|/) hlr divisor = map (/divisor) hlr

-- | A single scalar is divided by each element of a holor
(/|) :: (Unbox a, Fractional a) => a -> Holor a -> Holor a
{-# INLINE (/|) #-}
(/|) dividend hlr = map (dividend/) hlr

-- | elementwise holor addition
(|+|) :: (Unbox a, Num a) => Holor a -> Holor a -> Holor a
{-# INLINE (|+|) #-}
(|+|) h1 h2
    | (hShape h1) == (hShape h2) =
        Holor (hShape h1) (hStrides h1) (V.zipWith (+) (hValues h1) (hValues h2))
    | otherwise = error $ "(|+|) : Shape missmatch at elementwise holor addition"

-- | elementwise holor subtraction
(|-|) :: (Unbox a, Num a) => Holor a -> Holor a -> Holor a
{-# INLINE (|-|) #-}
(|-|) h1 h2
    | (hShape h1) == (hShape h2) =
        Holor (hShape h1) (hStrides h1) (V.zipWith (-) (hValues h1) (hValues h2))
    | otherwise = error $ "(|-|) : Shape missmatch at elementwise holor subtraction"

-- | elementwise holor multiplication
(|*|) :: (Unbox a, Num a) => Holor a -> Holor a -> Holor a
{-# INLINE (|*|) #-}
(|*|) h1 h2
    | (hShape h1) == (hShape h2) =
        Holor (hShape h1) (hStrides h1) (V.zipWith (*) (hValues h1) (hValues h2))
    | otherwise = error $ "(|*|) : Shape missmatch at elementwise holor multiplication"


-- | Holor multiplication, last dimension of left holor has to match first dimension of right holor
(|.|) :: (Unbox a, Num a) => Holor a -> Holor a -> Holor a
{-# INLINE (|.|) #-}
(|.|) h1 h2
    | dim h1 < 2 || dim h2 < 2 = error $ "(|.|) : holor dimensions have to be at least 2"
    | last (shape h1) /= matchdim =
        error $ "(|.|) : dimension mismatch: " ++ show (shape h1) ++ " , " ++ show (shape h2)
    | otherwise = Holor shapeOut stridesOut valuesOut
    where
        shape1 = take (length (shape h1) - 1) (shape h1)
        -- !shape1_ = D.trace ("shape1: " ++ show shape1) ()
        shape2 = drop 1 (shape h2)
        -- !shape2_ = D.trace ("shape2: " ++ show shape2) ()
        shapeOut = shape1 ++ shape2
        stridesOut = fromShapeToStrides shapeOut
        matchdim = head (shape h2) -- also step
        -- !matchdim_ = D.trace ("matchdim: " ++ show matchdim) ()
        h2T = transpose h2
        values1 = hValues h1
        values2 = hValues h2T
        valuesOut = V.fromList [
            let
                slice1 = V.unsafeSlice (i*matchdim) matchdim values1
                slice2 = V.unsafeSlice (j*matchdim) matchdim values2
            in
                V.sum (V.zipWith (*) slice1 slice2)
            | i <- [0 .. (product shape1 - 1)], j <- [0 .. (product shape2 - 1)]]


-- | Holor to a power elementwise
(|**) :: (Unbox a, Floating a) => Holor a -> a -> Holor a
{-# INLINE (|**) #-}
(|**) hlr p = map (**p) hlr

-- TODO: may be optimized by saving power of two products
-- | Holor multiplied with itself elementwise n-times
(|^) :: (Unbox a, Num a) => Holor a -> Int -> Holor a
{-# INLINE (|^) #-}
(|^) hlr num
    | num<0 = error $ "(|^) : number of multiplications must be non negative"
    | not (isSquare hlr) = error $ "(|^) : holor is not of square shape :" ++ show (hShape hlr)
    | num == 0 = eye $ (shape hlr) P.!! 0
    | otherwise = iterMult hlr 0
    where
        iterMult hlr_ n
            | n == 1 = iterMult hlr (n+1)
            | n <= num = iterMult (hlr |.| hlr_) (n+1)
            | otherwise = hlr_

----------------
-- ** Operations

-- | additive inverse of a holor
negate :: (Unbox a, Num a) => Holor a -> Holor a
{-# INLINE negate #-}
negate = map P.negate

-- | absolute values ​​by holor elements
abs :: (Unbox a, Num a) => Holor a -> Holor a
{-# INLINE abs #-}
abs= map P.abs

-- | signum by holor elements
signum :: (Unbox a, Num a) => Holor a -> Holor a
{-# INLINE signum #-}
signum = map P.signum

-- | maximum of all holor elements
maximum :: (Unbox a, Ord a) => Holor a -> a
{-# INLINE maximum #-}
maximum hlr = V.maximum $ hValues hlr

-- | minimum of all holor elements
minimum :: (Unbox a, Ord a) => Holor a -> a
{-# INLINE minimum #-}
minimum hlr = V.minimum $ hValues hlr

-- | sum all element of a holor
sum :: (Unbox a, Num a) => Holor a -> a
{-# INLINE sum #-}
sum hlr = V.sum $ hValues hlr

-- | trace of a matrix
trace :: (Unbox a, Num a) => Holor a -> a
{-# INLINE trace #-}
trace hlr
    | isSquare hlr = sum $ diagonal hlr
    | otherwise = error $
        "trace : holor with shape " ++ show (shape hlr) ++ " is not a square matrix"

-- | p-norm of a holor
norm :: (Unbox a, Real a, Unbox b, Floating b) => a -> Holor a -> b
{-# INLINE norm #-}
norm p hlr = (V.sum $ V.map (\v -> (realToFrac v)**p') (hValues hlr))**(1/p') where
    p' = realToFrac p

-- | manhattan norm of a holor (p = 1)
norm1 :: (Unbox a, Num a) => Holor a -> a
{-# INLINE norm1 #-}
norm1 hlr = V.sum $ V.map (\v -> P.abs v) (hValues hlr)

-- | frobenius norm of a holor. If the holor has vector form it's equal to the euclidian norm
norm2 :: (Unbox a, Real a, Unbox b, Floating b) => Holor a -> b
{-# INLINE norm2 #-}
norm2 hlr = sqrt $ V.sum $ V.map (\v -> (realToFrac v)**2) (hValues hlr)

-- | maximum norm of a holor (p = inf)
normM :: (Unbox a, Num a, Ord a) => Holor a -> a
{-# INLINE normM #-}
normM hlr = V.maximum $ V.map (\v -> P.abs v) (hValues hlr)

-- | normalize a holor to unit-length in p-norm
normalize :: (Unbox a, Real a, Floating a) => a -> Holor a -> Holor a
{-# INLINE normalize #-}
normalize p hlr = map (/(norm p hlr)) hlr

-- | normalize a holor to unit-length in 1-norm
normalize1 :: (Unbox a, Fractional a) => Holor a -> Holor a
{-# INLINE normalize1 #-}
normalize1 hlr = map (/(norm1 hlr)) hlr

-- | normalize a holor to unit-length in 2-norm
normalize2 :: (Unbox a, Real a, Floating a) => Holor a -> Holor a
{-# INLINE normalize2 #-}
normalize2 hlr = map (/(norm2 hlr)) hlr
-- | normalize a holor to unit-length in maximum-norm

normalizeM :: (Unbox a, Fractional a, Ord a) => Holor a -> Holor a
{-# INLINE normalizeM #-}
normalizeM hlr = map (/(normM hlr)) hlr

-- | dot product for holors with shape [n,1] (column-vector) or [1,n] (row-vector)
dot :: (Unbox a, Num a) => Holor a -> Holor a -> a
{-# INLINE dot #-}
dot v1 v2
    | not $ isVector v1 && isVector v2 =
        error $ "dot : arguments have no vector form"
    | shape1 /= shape2 =
        error $ "dot : shapes " ++ show (shape1) ++ " and " ++ show (shape2) ++ " incompatible"
    | otherwise =
        V.sum $ V.zipWith (*) (hValues v1) (hValues v2)
    where
        shape1 = shape v1
        shape2 = shape v2

-- | cross product for holors with shape [3,1] (column-vector) or [1,3] (row-vector)
cross :: (Unbox a, Num a) => Holor a -> Holor a -> Holor a
{-# INLINE cross #-}
cross v1 v2
    | not $ isVector v1 && isVector v2 =
        error $ "cross : both arguments must be a column or row vector"
    | not $ shape1 == shape2 && (shape1 == [3,1] || shape1 == [1,3]) =
        error $ "cross : shapes " ++ show (shape1) ++ " and " ++ show (shape2) ++ " incompatible"
    | otherwise = vector [v11*v22-v12*v21, v12*v20-v10*v22, v10*v21-v11*v20]
    where
        shape1 = shape v1
        shape2 = shape v2
        [v10, v11, v12] = toList v1
        [v20, v21, v22] = toList v2

{- TODO:
-- Link: https://en.wikipedia.org/wiki/Adjugate_matrix
-- | adjugate Matrix
adjugate :: (Unbox a, Num a) => Holor a -> Holor a
{-# INLINE adjugate #-}
adjugate hlr = undefined
-}

-- | adjugate of 2x2 matrix
adjugate22 :: (Unbox a, Num a) => Holor a -> Holor a
{-# INLINE adjugate22 #-}
adjugate22 mat22
    | shape mat22 == [2,2] = holor [2,2] values
    | otherwise = error ("adjugate22 only possible for a holor with shape [2,2]")
    where
        m00 =   mat22|!![0,0]
        m01 = - mat22|!![1,0]
        m10 = - mat22|!![0,1]
        m11 =   mat22|!![1,1]
        values = [m11, m10, m01, m00]

-- | adjugate of 3x3 matrix
adjugate33 :: (Unbox a, Num a) => Holor a -> Holor a
{-# INLINE adjugate33 #-}
adjugate33 mat33
    | shape mat33 == [3,3] = holor [3,3] values
    | otherwise = error ("adjugate33 only possible for a holor with shape [3,3]")
    where
        h00 = mat33|!![0,0]; h01 = mat33|!![0,1]; h02 = mat33|!![0,2]
        h10 = mat33|!![1,0]; h11 = mat33|!![1,1]; h12 = mat33|!![1,2]
        h20 = mat33|!![2,0]; h21 = mat33|!![2,1]; h22 = mat33|!![2,2]
        m00 = h11*h22 - h12*h21
        m01 = h12*h20 - h10*h22
        m02 = h10*h21 - h11*h20
        m10 = h02*h21 - h01*h22
        m11 = h00*h22 - h02*h20
        m12 = h01*h20 - h00*h21
        m20 = h01*h12 - h02*h11
        m21 = h02*h10 - h00*h12
        m22 = h00*h11 - h01*h10
        values = [m00, m10, m20, m01, m11, m21, m02, m12, m22]

-- Link: https://semath.info/src/inverse-cofactor-ex4.html
-- (Adj44)_ij = (-1)^(i+j) * det33(M_ji)
-- | adjugate of 4x4 matrix
adjugate44 :: (Unbox a, Num a) => Holor a -> Holor a
{-# INLINE adjugate44 #-}
adjugate44 mat44
    | shape mat44 == [4,4] = holor [4,4] values
    | otherwise = error ("adjugate44 only possible for a holor with shape [4,4]")
    where
        h00 = mat44|!![0,0]; h01 = mat44|!![0,1]; h02 = mat44|!![0,2]; h03 = mat44|!![0,3]
        h10 = mat44|!![1,0]; h11 = mat44|!![1,1]; h12 = mat44|!![1,2]; h13 = mat44|!![1,3]
        h20 = mat44|!![2,0]; h21 = mat44|!![2,1]; h22 = mat44|!![2,2]; h23 = mat44|!![2,3]
        h30 = mat44|!![3,0]; h31 = mat44|!![3,1]; h32 = mat44|!![3,2]; h33 = mat44|!![3,3]
        m00 =   (det33 $ holor [3,3] [h11,h12,h13,h21,h22,h23,h31,h32,h33])
        m01 = - (det33 $ holor [3,3] [h10,h12,h13,h20,h22,h23,h30,h32,h33])
        m02 =   (det33 $ holor [3,3] [h10,h11,h13,h20,h21,h23,h30,h31,h33])
        m03 = - (det33 $ holor [3,3] [h10,h11,h12,h20,h21,h22,h30,h31,h32])
        m10 = - (det33 $ holor [3,3] [h01,h02,h03,h21,h22,h23,h31,h32,h33])
        m11 =   (det33 $ holor [3,3] [h00,h02,h03,h20,h22,h23,h30,h32,h33])
        m12 = - (det33 $ holor [3,3] [h00,h01,h03,h20,h21,h23,h30,h31,h33])
        m13 =   (det33 $ holor [3,3] [h00,h01,h02,h20,h21,h22,h30,h31,h32])
        m20 =   (det33 $ holor [3,3] [h01,h02,h03,h11,h12,h13,h31,h32,h33])
        m21 = - (det33 $ holor [3,3] [h00,h02,h03,h10,h12,h13,h30,h32,h33])
        m22 =   (det33 $ holor [3,3] [h00,h01,h03,h10,h11,h13,h30,h31,h33])
        m23 = - (det33 $ holor [3,3] [h00,h01,h02,h10,h11,h12,h30,h31,h32])
        m30 = - (det33 $ holor [3,3] [h01,h02,h03,h11,h12,h13,h21,h22,h23])
        m31 =   (det33 $ holor [3,3] [h00,h02,h03,h10,h12,h13,h20,h22,h23])
        m32 = - (det33 $ holor [3,3] [h00,h01,h03,h10,h11,h13,h20,h21,h23])
        m33 =   (det33 $ holor [3,3] [h00,h01,h02,h10,h11,h12,h20,h21,h22])
        values = [m00,m10,m20,m30,m01,m11,m21,m31,m02,m12,m22,m32,m03,m13,m23,m33]

{- TODO:
det :: (Unbox a, Num a) => Holor a -> a
{-# INLINE det #-}
det hlr
    | (length (shape hlr) == 2) && ((shape hlr) P.!! 0 == (shape hlr) P.!! 1 = result
    | otherwise = error $ "det of nonsquare holor with shape " ++ show (shape hlr)
    where
        result = undefined
-}


-- | determinant of 2x2 matrix
det22 :: (Unbox a, Num a) => Holor a -> a
{-# INLINE det22 #-}
det22 mat22
    | shape mat22 == [2,2] = h00*h11 - h01*h10
    | otherwise = error ("det22 only possible for a holor with shape [2,2]")
    where
        h00 = mat22|!![0,0]
        h01 = mat22|!![0,1]
        h10 = mat22|!![1,0]
        h11 = mat22|!![1,1]

-- Laplace-Expansion by first row
-- | determinant of 3x3 matrix
det33 :: (Unbox a, Num a) => Holor a -> a
{-# INLINE det33 #-}
det33 mat33
    | shape mat33 == [3,3] = h00*(det22 m00) - h01*(det22 m01) + h02*(det22 m02)
    | otherwise = error ("det33 only possible for a holor with shape [3,3]")
    where
        h00 = mat33|!![0,0]
        h01 = mat33|!![0,1]
        h02 = mat33|!![0,2]
        m00 = mat33|||!![[1,2],[1,2]]
        m01 = mat33|||!![[1,2],[0,2]]
        m02 = mat33|||!![[1,2],[0,1]]

-- Laplace-Expansion by first row
-- | determinant of 4x4 matrix
det44 :: (Unbox a, Num a) => Holor a -> a
{-# INLINE det44 #-}
det44 mat44
    | shape mat44 == [4,4] = h00*(det33 m00) - h01*(det33 m01) + h02*(det33 m02) - h03*(det33 m03)
    | otherwise = error ("det44 only possible for a holor with shape [4,4]")
    where
        h00 = mat44|!![0,0]
        h01 = mat44|!![0,1]
        h02 = mat44|!![0,2]
        h03 = mat44|!![0,3]
        m00 = mat44|||!![[1,2,3],[1,2,3]]
        m01 = mat44|||!![[1,2,3],[0,2,3]]
        m02 = mat44|||!![[1,2,3],[0,1,3]]
        m03 = mat44|||!![[1,2,3],[0,1,2]]


{- TODO:
inv :: (Unbox a, Fractional a) => Holor a -> Holor a
{-# INLINE inv #-}
inv hlr = error "not implemented"
-}

-- Link: https://de.wikipedia.org/wiki/Inverse_Matrix#Explizite_Formeln
-- | inverse of 2x2 matrix
inv22 :: (Unbox a, Fractional a) => Holor a -> Holor a
{-# INLINE inv22 #-}
inv22 mat22
    | shape mat22 == [2,2] = (1/det22 mat22) *| (adjugate22 mat22)
    | otherwise = error ("inv22 only possible for a holor with shape [2,2]")

-- Link: https://de.wikipedia.org/wiki/Inverse_Matrix#Explizite_Formeln
-- | inverse of 3x3 matrix
inv33 :: (Unbox a, Fractional a) => Holor a -> Holor a
{-# INLINE inv33 #-}
inv33 mat33
    | shape mat33 == [3,3] = (1/det33 mat33) *| (adjugate33 mat33)
    | otherwise = error ("inv33 only possible for a holor with shape [3,3]")

-- Link: https://semath.info/src/inverse-cofactor-ex4.html
-- | inverse of 4x4 matrix
inv44 :: (Unbox a, Fractional a) => Holor a -> Holor a
{-# INLINE inv44 #-}
inv44 mat44
    | shape mat44 == [4,4] = (1/det44 mat44) *| (adjugate44 mat44)
    | otherwise = error ("inv44 only possible for a holor with shape [4,4]")


-- | select all indices of a holor with nonzero elements
selectNonzero :: (Unbox a, Num a, Eq a) => Holor a -> [[Int]]
{-# INLINE selectNonzero #-}
selectNonzero hlr = selectBy (/=0) hlr

-- | number of nonzero elements
countNonzero :: (Unbox a, Num a, Eq a) => Holor a -> Int
{-# INLINE countNonzero #-}
countNonzero hlr = countBy (/=0) hlr



