-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Nuha.Algorithms where

import Prelude
import qualified Prelude as P
import Data.Vector.Unboxed (Unbox)
import Math.Nuha.Base
import Math.Nuha.Numeric
import Math.Nuha.Types

-- LITERATURE:
--  [1]: "Einführung in die Numerische Mathematik - Thomas Richter - 3.Auflage"

-------------
-- ** Solvers

{- | Solves the linear system A*x=b with A as a square matrix. The possible errors that can happen are 'NoSquareMatrixError', 'UnderdeterminedSystemError' and 'DimensionMismatchError'

> _A = matrix [[1,1,2],[2,-3,0],[2,4,-4]]
> b = vector [2,5,3]
> x = case solveLin _A b of
>     Left err -> error $ "solveLin : " ++ show err
>     Right x -> x
-}
solveLin :: (Unbox a, Real a, Floating a)
    => Holor a
    -> Holor a -> Either Error (Holor a)
solveLin _A b = case isSquare _A of
    False -> Left NoSquareMatrixError
    True -> case solveLinLS _A b of
        Left UnderdeterminedSystemError -> Left UnderdeterminedSystemError
        Left DimensionMismatchError -> Left DimensionMismatchError
        Right x -> Right x

{-
With the QR-decomposition of A the solution can be found like this:
    A*x=b <=> Q*R*x=b <=> R*x=Q.T*b
The last equation can be solved easily by backward substitution.
We actually don't need the full QR-decomposition of A. It suffices if we have the householder reflection vectors from the incomplete QR decomposition. With that the right hand side of the equation can be expressed as:
    Q.T*b = S_(m-1)*S_(m-2)*...*S_1*b
For further explanation see [1], P.73
-}
{- | Solves the linear least squares problem, i.e. finds the least squares solution of the overdetermined linear equation system A*x=b. Possible errors are 'UnderdeterminedSystemError' and 'DimensionMismatchError'

> _A = matrix [[1,1,2],[2,-3,0],[2,4,-4],[2,4,-4.3]]
> b = vector [2,3,4,5]
> x = case solveLinLS _A b of
>     Left err -> error $ "solveLinLS : " ++ show err
>     Right x -> x
-}
solveLinLS :: (Unbox a, Real a, Floating a) => Holor a -> Holor a -> Either Error (Holor a)
solveLinLS _A b = case facPreQR _A of
    Left TooFewRowsError -> Left UnderdeterminedSystemError
    Left NoMatrixError -> Left DimensionMismatchError
    Right (listV, _R) -> result where
        -- The product of a matrix S_j and a vector b can be expressed as a reflection with the corresponding housholder reflection vector v_j, see [1], S.71:
        reflection :: (Unbox a, Num a) => Holor a -> Holor a -> Holor a
        reflection v b = b |-| 2*(dot v b) *| v
        -- If the shapes of A and b match, the solution can be found by backward substitution in the System R*x=Q.T*b, see ([1], P.83):
        result = case hShape b == [m,1] of
            True -> case solveLinBack _R _QTb of
                Left NoUpperTriError -> Left UnderdeterminedSystemError
                Left DimensionMismatchError -> Left DimensionMismatchError
                Left NoMatrixError -> Left DimensionMismatchError
                Right x -> Right x
            False -> Left DimensionMismatchError
            where
                m = head $ hShape _A
                _QTb = foldr reflection b listV


-- see [1], S.34
-- | Backward substitution for solving R*x=b with an upper triangular matrix R. Possible errors are 'DimensionMismatchError' and 'NoUpperTriError'
solveLinBack ::  (Unbox a, Real a, Floating a) => Holor a -> Holor a -> Either Error (Holor a)
solveLinBack _R b = case hShape _R of
    [m,n] -> case isUpperTri _R of
        False -> Left NoUpperTriError
        True -> case hShape b == [m,1] of
            False -> Left DimensionMismatchError
            True -> Right $ recursion (n-1) []
            where
                recursion i xList = case i==0 of
                    True -> vector xList'
                    False -> recursion (i-1) xList'
                    where
                        xList' = xi : xList
                        xi = 1/(_R|![i,i]) *
                            (b|![i,0] -
                                P.sum (zipWith (*) (toList (_R|||![[i],[i+1..n-1]])) xList))
    _ -> Left DimensionMismatchError

--------------------
-- ** Factorizations

{- | Factorization of a rectangular matrix A with shape [m,n] where __m>=n__ into the matrices (Q,R) with A=Q*R where Q is orthogonal and R is of shape [m,n] with the first n rows in upper triangular form and the last m-n rows filled with zeros. Possible errors are 'TooFewRowsError' and 'NoMatrixError'

> _A = matrix [[1,1,2],[2,-3,0],[2,4,-4],[2,4,-5]]
> (_Q,_R) = case facQR _A of
>     Left err -> error $ "facQR : " ++ show err
>     Right (_Q,_R) -> (_Q,_R)
-}
facQR :: (Unbox a, Real a, Floating a) => Holor a -> Either Error (Holor a, Holor a)
facQR _A = case facPreQR _A of
    Left TooFewRowsError -> Left TooFewRowsError
    Left NoMatrixError -> Left NoMatrixError
    Right (listV, _R) -> Right (_Q,_R) where
        _Q = transpose $
            foldr (\v _S -> (eye m |-| 2*| (v |.| transpose v)) |.| _S) (eye m) listV
        [m,n] = hShape _A


{- | Pre step of the QR factorization of a rectangular matrix A. The outputs of this function are a the list of the householder reflection vectors and the matrix R which is the same as in the full QR factorization. The matrix Q is implicitly stored in the householder reflection vectors by the rule: transpose(Q) = S_m * ... * S_1. For the matrices S_i applies: S_i = I - 2*(dot v_i v_i) where I is the Identity matrix and v_i the i-th householder vector. This function is sometimes useful where the Q Matrix isn't explicitly needed. Possible errors are 'TooFewRowsError' and 'NoMatrixError'

> _A = matrix [[1,1,2],[2,-3,0],[2,4,-4],[2,4,-5]]
> (reflectionVectors,_R) = case facPreQR _A of
>     Left err -> error $ "facPreQR : " ++ show err
>     Right (reflectionVectors,_R) -> (reflectionVectors,_R)
-}
facPreQR :: (Unbox a, Real a, Floating a) => Holor a -> Either Error ([Holor a], Holor a)
facPreQR _A = case hShape _A of
    [m,n] -> guards where
        guards
            | m<n = Left TooFewRowsError
            | m==n = Right (listV, _R)
            | otherwise = Right (listV', _R')
        -- For a square input matrix the recursion is called without an additional recursion step:
        (listV, listR) = recursion (m,n) False [] [] (Just _A)
        _R = matrix $ reverse listR
        -- For a rectangular input matrix there must me an additional recursion step. Notice that in this case there must be additional rows with zeros in the R-Matrix to go conform with the shape of Q:
        (listV', listR') = recursion (m,n) True [] [] (Just _A)
        _R' = matrix $ reverse (take (m-n) (repeat (take n (repeat 0))) ++ listR')
    _ -> Left NoMatrixError
    where
        -- recursion to calculate the householder vectors and the elements of the R-Matrix, see [1], S.70ff. The parameter shapeIn is the shape of the input Matrix A. The parameter additionalStep determines if there should be an additional recursion step, which is the case for rectangular input matrices with more rows than columns:
        recursion :: (Unbox a, Real a, Floating a) =>
            (Int,Int) -> Bool -> [Holor a] -> [[a]] -> Maybe (Holor a) -> ([Holor a], [[a]])
        recursion shapeIn additionalStep listV listR maybeA = case maybeA of
            Nothing -> (listV, listR)
            Just _A -> recursion shapeIn additionalStep' listV' listR' maybeA' where
                -- shapes of the Input matrix and the submatrix of the current recursion step
                (rows,cols) = shapeIn
                [m,n] = hShape _A
                -- top left element and first colomn of _A:
                a00 = _A|!![0,0]
                a_0 = _A|||!![[0..m-1],[0]]
                -- next reflection vector, notice that the standard signum function can be zero, so a customized one must be used which can only bei -1 or 1:
                sign x = if x<0 then -1 else 1
                v = normalize2 $ a_0 |+| vector ((sign a00)*(norm2 a_0) : (take (m-1) (repeat 0)))
                -- append leading zeros to the reflection vector to make sure all reflection vectors are of the same length and add it to the list of the existing ones:
                listV' = (vector (take (rows - numElems v) (repeat 0) ++ toList v)) : listV
                -- calculate the new list with the rows of the R-Matrix and maybe a new submatrix A'. If there is no next submatrix A', the recursion will stop at the begin of the next recursion:
                i = length listV -- recursion step
                (listR', maybeA', additionalStep') = case i<cols-1 of
                    -- Do a standard recursion step. A new row of R is appended as the first row of S*A with leading zeros and the new submatrix A' is taken from S*A by skipping the first row and first column, see [1], P.71:
                    True ->
                        ( (take i (repeat 0) ++ (_SA00 : (head _SA_withoutFirstColumn))) : listR
                        , Just $ matrix (tail _SA_withoutFirstColumn)
                        , additionalStep )
                    -- The last row of R is calculated different, depending if there should be an additional step at the end:
                    False -> case additionalStep of
                        -- If there should be no additional step the new row of R is calculated as the first row of S*A with leading zeros (which has only the last element as nonzero). There will be no next submatrix A':
                        False ->
                            ( (take (cols-1) (repeat 0) ++ [_SA00]) : listR
                            , Nothing
                            , False )
                        -- If there should be an additional step, the matrix R doesn't change, but there is a next special submatrix A', see ([1], P.82-84). In addition the parameter additionalStep is set to False to finish at the next recursion:
                        True ->
                            ( listR
                            , Just $ _SA_firstColumnHolor
                            , False )
                    where
                        -- first element of the first column of S*A:
                        _SA00 = - (sign a00) * (norm2 a_0)
                        -- second to last column of S*A (from index j = 1 to n-1) as a nested list of rows:
                        _SA_withoutFirstColumn = toList2 $ transpose $ matrix [
                            let
                                a = _A|||!![[0..m-1],[j]]
                            in
                                toList $ a |-| 2*(dot v a) *| v
                            | j <- [1..n-1]]
                        -- new submatrix for the additional recursion step:
                        _SA_firstColumnHolor = a |-| 2*(dot v a) *| v where
                            a = _A|||!![[0..m-1],[0]]

