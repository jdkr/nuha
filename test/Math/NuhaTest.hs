{-# LANGUAGE BangPatterns #-}

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

{- Developer Information:
For Runtime Testing do the following:
    - comment in the dependency 'clock' to build-depends of the test-suite in the file nuha.cabal
    - comment in the import line for System.Clock in this file
    - comment in the functions 'toSec' and 'testRuntime' in this file
This way is choosen, because this package should not depend on the clock package by default.
Note: Runtime will be shorter if this file is compiled with ghc instead of interpreted by ghci.
-}

{-
Links:
https://chrisdone.com/posts/measuring-duration-in-haskell/
https://stackoverflow.com/questions/14163072/how-to-force-evaluation-in-haskell
-}

module Math.NuhaTest where

-- import System.Clock (Clock(Monotonic), TimeSpec(..), toNanoSecs, getTime )
import Math.Nuha.Types
import Math.Nuha.Base
import Math.Nuha.Numeric
import Math.Nuha.Internal
import Math.Nuha.Algorithms


inv44Test :: Holor Double
inv44Test = (inv44 a) |.| a where
    a = holor [4,4] [3,4,6,2,4,7,9,5,2,3,7,9,5,2,2,4]

inv33Test :: Holor Double
inv33Test = inv33 a |.| a where
    a = holor [3,3] [0,1,2,3,4,5,6,7,9]

testAdj44 :: Holor Double
testAdj44 = adjugate44 a where
    a = holor [4,4] [3,4,6,2,4,7,9,5,2,3,7,9,5,2,2,4]

cartProdTest :: [[Int]]
cartProdTest =
    cartProd [[1,2],[11,22],[111,222]]
    -- cartProd [[0..999],[0..999]]

transposeTest :: Holor Int
transposeTest = transpose a where
    -- a = holor [3,2,2] [1, 2, 3, 4, 11, 22, 33, 44, 111, 222, 333, 444]
    -- a = holor [4,3,2,2] [1, 2, 3, 4, 11, 22, 33, 44, 111, 222, 333, 444, 1, 2, 3, 4, 11, 22, 33, 44, 111, 222, 333, 444, 1, 2, 3, 4, 11, 22, 33, 44, 111, 222, 333, 444, 1, 2, 3, 4, 11, 22, 33, 44, 111, 222, 333, 444]
    -- a = holor [1000,1000] [0..999999]
    a = holor [5,5] [0..24]

multTest :: Holor Double
multTest = b |.| b where
    b = matrix [[2,3,2], [5,2,6], [1,6,3]]
    -- b = holor [10,10] [0..99]
    -- b = holor [100,100] [0..9999]
    -- b = holor [500,500] [0..249999]
    -- b = holor [1000,1000] [0..999999]


facQRTest :: (Holor Double, Holor Double)
facQRTest = (_Q,_R) where
    -- _A = matrix [[1,1,2],[2,-3,0],[2,4,-4]]
    -- _A = matrix [[1,1,2],[2,-3,0],[2,-3,0],[2,-3,0]]
    -- _A = matrix [[1,1,2],[2,-3,0],[2,-3,0]]
    -- _A = matrix [[1,1,2],[2,-3,0],[2,4,-4],[2,4,-5],[9,3,0]]
    -- _A = matrix [[1,1,2],[2,-3,0],[2,-3,0],[1,1,2]]
    -- _A = matrix [[1,1,2],[1,1,2],[3,3,3]]
    _A = matrix [[1,1,2],[2,-3,0],[2,4,-4],[2,4,-5]]
    (_Q,_R) = case facQR _A of
        Left err -> error $ "facQR : " ++ show err
        Right (_Q,_R) -> (_Q,_R)


facPreQRTest :: ([Holor Double], Holor Double)
facPreQRTest = (reflectionVectors,_R) where
    -- _A = matrix [[1,1,2],[2,-3,0],[2,4,-4]]
    -- _A = matrix [[1,1,2],[2,-3,0],[2,4,-4],[2,4,-5]]
    -- _A = matrix [[1,1,2],[2,-3,0],[2,4,-4],[2,4,-5],[9,3,0]]
    -- _A = matrix [[1,1,2],[2,-3,0],[2,-3,0]]
    -- _A = matrix [[1,1,2],[2,-3,0]]
    -- _A = matrix [[1,1,2],[1,1,2],[1,1,2]]
    _A = matrix [[1,1,2],[2,-3,0],[2,4,-4],[2,4,-5]]
    (reflectionVectors,_R) = case facPreQR _A of
        Left err -> error $ "facPreQR : " ++ show err
        Right (reflectionVectors,_R) -> (reflectionVectors,_R)

solveLinTest :: Holor Double
solveLinTest = x where
    _A = matrix [[1,1,2],[2,-3,0],[2,4,-4]]
    -- _A = matrix [[1,1,2],[2,-3,0],[2,-3,0]]
    -- _A = matrix [[1,1,2],[2,-3,0],[2,4,-4],[2,3,1]]
    b = vector [2,5,3]
    x = case solveLin _A b of
        Left err -> error $ "solveLin : " ++ show err
        Right x -> x


solveLinLSTest :: Holor Double
solveLinLSTest = x where
    _A = matrix [[1,1,2],[2,-3,0],[2,4,-4],[2,4,-4.3]]
    -- b = vector [2,3,4,4]
    -- _A = matrix [[1,1,2],[2,-3,0],[2,4,-4]]
    -- _A = matrix [[1,1,2],[2,-3,0],[2,-3,0]]
    b = vector [2,3,4,5]
    x = case solveLinLS _A b of
        Left err -> error $ "solveLinLS : " ++ show err
        Right x -> x


sliceTest :: Holor Double
sliceTest = slice where
    _A = holor [4,4] [1..16]
    -- _A = vector [1,1,2,2,-3,0,2,4,-4,2,4,-5,9,3,0]
    -- _A = matrix [[1,1,2],[2,-3,0],[2,4,-4],[2,4,-5],[9,3,0]]
    -- _A = holor [4,3,2] [1 .. 24]
    slice = _A||!![1,1,3]


test = do
    -- let a = multTest
    -- let a = inv33Test
    -- let a = inv44Test
    -- let a = transposeTest
    -- let a = cartProdTest

    -- let _S = sliceTest
    -- putStrLn $ "show _S"
    -- putStrLn $ show _S


    -- let facPreQRResult = facPreQRTest
    -- putStrLn $ "show facPreQRResult"
    -- putStrLn $ show facPreQRResult

    -- let facQRResult = facQRTest
    -- putStrLn $ "show facQRResult"
    -- putStrLn $ show facQRResult

    let x = solveLinTest
    putStrLn $ "solveLinTest:"
    putStrLn $ show x

    let x = solveLinLSTest
    putStrLn $ "solveLinLSTest:"
    putStrLn $ show x

    let (_Q,_R) = facQRTest
    putStrLn $ "facQRTest:"
    putStrLn $ show (_Q,_R)

    let (reflectionVectors,_R) = facPreQRTest
    putStrLn $ "facPreQRTest:"
    putStrLn $ show (reflectionVectors,_R)


-- -- Convenience Function for converting a TimeSpec from the clock-package
-- toSecs :: TimeSpec -> Double
-- toSecs timeSpec = secs where
--     nanoSecs = toNanoSecs timeSpec
--     secs = (fromIntegral nanoSecs) / 10^9
--
--
-- testRuntime = do
--     start <- getTime Monotonic
--     let !a = multTest
--     let !a = inv33Test
--     let !a = inv44Test
--     let !a = transposeTest
--     let !a = cartProdTest
--     end <- getTime Monotonic
--     -- putStrLn $ show a
--     putStrLn $ show $ toSecs (end - start)
