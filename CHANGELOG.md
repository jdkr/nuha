# Revision history for nuha

## 0.1.0.0 -- 2020-03-10

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2020-04-08

* Renamed Array to Holor
* Added functions for normalization
* Change of Vector type to Holor type in several functions
* Added integer indexing for column and row vectors
* Changed name of indexing functions
* Added holor division by a scalar
* Added cross product for vectors
* Added functions toList and toList2
* Renamed sizeItem and sizeItems and numItems to sizeOfElem, sizeOfElems and numElems
* Added Functions (|+), (+|), (|-), (-|), (|/), (/|)
* Added instance Ord
* Renamed setValue and setValues to setElem and setElems
* Added constructor functions from tuples for low dimensional holors
* Added conversion functions to tuple types

## 0.3.0.0 -- 2020-11-27

* Bugfix: disallowed empty holors in holor generating functions
* Renamed multidemensional indexing from (||!) and (||!!) to (|||!) and (|||!!)
* Added holor slicing functions (||!) and (||!!)
* Added function for to test an upper triangular matrix
* Added function for backward substitution
* Added functions for QR decomposition
* Added function for solving linear equation systems
* Added function for solving the linear least squares problem
* Changed parameter order in functions reshape, setElem and setElems

## 0.X.X.X
* TODO: Check bounds of dependencies
* TODO: Maybe create Type synonyms for Idx, Idcs, MIdx and MIdcs. Are there even other types possible than lists to get more performance by similar user friendliness? Does the tensor package on hackage have there some ideas?