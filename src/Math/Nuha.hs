-- |
-- Module      : Math.Nuha
-- Copyright   : (c) Johannes Kropp 2020
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Nuha is a library for linear algebra based on multidimensional arrays. The fundamental container is @Data.Vector.Unboxed@.
--
-- The goals of Nuha are:
--
-- * to be user friendly
--
-- * to be lightweight with few dependencies
--
-- * to be fast (although there haven't been many benchmarks yet)
--
-- If there's time in the future, it is planned to implement algorithms of numerical analysis, e.g. solve linear systems, determinant, inverse, svd, eigenvalues, matrix decompositions ...
--

-- Please visit also my <https://github.com/jdkr/nuha github-repository> for further information on this project.


module Math.Nuha
    ( -- * Math.Nuha.Types
      module Math.Nuha.Types
      -- * Math.Nuha.Base
    , module Math.Nuha.Base
      -- * Math.Nuha.Numeric
    ,  module Math.Nuha.Numeric
      -- * Math.Nuha.Algorithms
    , module Math.Nuha.Algorithms
    ) where


import Math.Nuha.Types
import Math.Nuha.Base
import Math.Nuha.Numeric
import Math.Nuha.Algorithms