{-# LANGUAGE BangPatterns #-}

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

{-
Links:
https://chrisdone.com/posts/measuring-duration-in-haskell/
https://stackoverflow.com/questions/14163072/how-to-force-evaluation-in-haskell
-}

module Math.NuhaTest where

import System.Clock (Clock(Monotonic), TimeSpec(..), toNanoSecs, getTime )
import qualified Data.Vector.Unboxed as V

import Math.Nuha.Instances
import Math.Nuha.Types
import Math.Nuha.Base
import Math.Nuha.Numeric
import Math.Nuha.Internal


testInv44 :: Array Double
testInv44 = (inv44 a) |.| a where
    a = array [4,4] [3,4,6,2,4,7,9,5,2,3,7,9,5,2,2,4]

testInv33 :: Array Double
testInv33 = inv33 a |.| a where
    a = array [3,3] [0,1,2,3,4,5,6,7,9]

testAdj44 :: Array Double
testAdj44 = adjugate44 a where
    a = array [4,4] [3,4,6,2,4,7,9,5,2,3,7,9,5,2,2,4]

testCartprod :: [[Int]]
testCartprod = cartProd [[0..999],[0..999]]

testTranspose :: Array Int
testTranspose = transpose a where
    -- a = array [3,2,2] [1, 2, 3, 4, 11, 22, 33, 44, 111, 222, 333, 444]
    -- a = array [4,3,2,2] [1, 2, 3, 4, 11, 22, 33, 44, 111, 222, 333, 444, 1, 2, 3, 4, 11, 22, 33, 44, 111, 222, 333, 444, 1, 2, 3, 4, 11, 22, 33, 44, 111, 222, 333, 444, 1, 2, 3, 4, 11, 22, 33, 44, 111, 222, 333, 444]
    a = array [1000,1000] [0..999999]

testMult :: Array Double
testMult = b |.| b where
    -- b = array2d [[2,3,2], [5,2,6], [1,6,3]]
    -- b = array [10,10] [0..99]
    -- b = array [100,100] [0..9999]
    -- b = array [500,500] [0..249999]
    b = array [1000,1000] [0..999999]


-- Convenience Function for converting a TimeSpec from the clock-package
toSecs :: TimeSpec -> Double
toSecs timeSpec = secs where
    nanoSecs = toNanoSecs timeSpec
    secs = (fromIntegral nanoSecs) / 10^9


main = do
    start <- getTime Monotonic
    let !a = testMult
    -- let !a = testInv33
    -- let !a = testIndex
    -- let !a = testTranspose
    -- let !a = testCartprod
    -- let !a = cartprod [[1,2],[11,22],[111,222]]
    end <- getTime Monotonic
    -- putStrLn $ show a
    putStrLn $ show $ toSecs (end - start)



