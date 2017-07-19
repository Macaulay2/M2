doc ///
Key
  PruneComplex
Headline
  Pruning chain complexes over polynomial and local rings
Description
  Text
    Define pruning ...
    
    This package includes various methods for pruning chain complexes over polynomial rings and local
    rings. ...
  Example
    "general usage example"
Caveat
  Does not work with general localization, only localization at prime ideals.
SeeAlso
  "LocalRings"
  "pruneComplex"
///


doc ///
Key
  UnitTest
///
///
Headline
Usage
Description
  Text
  Example
Caveat
SeeAlso
///


doc ///
Key
  left
///
///
Headline
Usage
Description
  Text
  Example
Caveat
SeeAlso
///


doc ///
Key
  right
///
///
Headline
Usage
Description
  Text
  Example
Caveat
SeeAlso
///


doc ///
Key
  both
///
///
Headline
Usage
Description
  Text
  Example
Caveat
SeeAlso
///


doc ///
Key
  whole
///
///
Headline
Usage
Description
  Text
  Example
Caveat
SeeAlso
///



--Ideal -> ChainComplex
-- Warning: only works for finite fields
-- homogenizes the ideal w.r.t. a new variable, calculates a free resolution using res 
-- with option FastNonminimal, then dehomogenizes
doc ///
Key
  freeRes
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///

-- converts a ChainComplex into a list of mutable matrices
doc ///
Key
  toMutable
  (toMutable, ChainComplex)
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///



-- converts a list of mutable matrices into a ChainComplex. Problems:
--  a. what if #mutableComplex == 0?
--  b. possibly want gradings kept in the chain complex, if homogeneous.
-- chainComplex for m in mutableComplex list map(R^(numRows m), R^(numColumns m), matrix m)
doc ///
Key
  toChainComplex
  (toChainComplex, List)
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///

-- Moves a given row and column to the end while keeping the neighbouring differentials compatible
-- Note: should enable moving a list of units to a square in the end
doc ///
Key
  moveToEnd
  (moveToEnd, List, ZZ, Sequence)
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///

-- uses liftable(f, coefficientRing ring f)
doc ///
Key
  isScalar
  (isScalar, RingElement)
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///

--  = method(Options => {UnitTest => isUnit})
-- A mutable matrix and a pair of coordinates -> a pair of coordinates for a unit or null
-- start scanning at this location, row by row, return the coordinates of the first unit or scalar
-- start from the beginning if no coordinate is givven
-- option: uses the unit testing function specified by UnitTest, default is isUnit, can also choose isScalar
-- caveat: how to find bigger than 1x1 units?
doc ///
Key
  findUnit
  (findUnit, MutableMatrix)
  (findUnit, MutableMatrix, Sequence)
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///

--  = method(Options => options findUnit)
-- A mutable matrix -> List (of coordinates for all units or scalars in a matrix)
-- same options as findUnit
doc ///
Key
  findAllUnits
  (findAllUnits, MutableMatrix)
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///

-- = method(Options => options findUnit)
--   := opts -> M -> ((int, int), int)
-- A mutable matrix -> a coordinate pair and a complexity integer
-- the same options as findUnit
-- lists all units, uses heuristics to calculate a complexity for each unit, then returns the simplest
-- uses different measures for poly ring and local ring; for poly ring returns the coordinates of the
-- unit with the sparsest row and column in a matrix; for local ring takes into account the sum of 
-- number of terms in the numerators for all elements in row and column as well.
-- caveat: super time intensive
-- Note: if no unit found, returns ((-1,-1), infinity)
doc ///
Key
  findSparseUnit
  (findSparseUnit, MutableMatrix)
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///

-- = method(Options => options findUnit)
-- Input: a list of mutable matrices mutableComplex followed by an iterator loc
-- Output: a list of mutable matrices mutableComplex of smaller size
-- reduce mutableComplex#loc using the unit in the last row and column, then remove the last 
-- row/column and correesponding row/column of adjacent matrices
-- caveat: only works with 1x1 units
-- note: different strategy for poly ring and local ring
--    inversePivot := if instance(ring M, LocalRing) then 
--        1/(M_(rlast, clast))
--      else 
--        1/(leadCoefficient M_(rlast, clast));
--    for c from 0 to clast - 1 do columnAdd(M, c, -M_(rlast, c) * inversePivot, clast);
doc ///
Key
  pruneUnit
  (pruneUnit, List, ZZ) 
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///

-- Prunes a single differential by reducing the unit, starting with the ones in sparcest row/column.
-- does a while loop until no units are left.
-- first moves a unit to the end of the differential, then calls pruneUnit
--  = method(Options => options findUnit)
doc ///
Key
  pruneDiff
  (pruneDiff, ChainComplex, ZZ)
  (pruneDiff, List, ZZ)
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///


doc ///
Key
  pruneComplex
  (pruneComplex, ChainComplex)
  (pruneComplex, List)
Headline
  Prunes a chain complex or list of mutable matrices
Usage
  D = pruneComplex C
Inputs
  C: ChainComplex
    or a list of mutable matrices defining the differentials of a complex
Outputs
  D: ChainComplex
    or the list of modified mutable matrices
Description
  Text
    -- Prune a chain complex C into a free resolution by removing unit elements from the differentials
    Options:
    UnitTest => isUnit --default. other option: isScalar
    Strategy => left   --default. pruning the right one first
    Strategy => right  -- pruning the left one first
    Strategy => both   -- pruning outside-in
    Strategy => whole  -- pruning the sparsest unit
  Example
    R = ZZ/32003[a..f]
    I = ideal"abc-def,ab2-cd2-c,acd-b3-1"
    C = freeRes I
    D = pruneComplex(C, UnitTest => isScalar, Strategy => both)
    C.dd
    D.dd
Caveat
  For inhomogeneous input the resulting complex is not guaranteed to be minimal, particularly because
  minimality is not well-defined in that case.
SeeAlso
  freeRes
  pruneDiff
///


-- TO BE MERGED IN isHomotopic
-- caveat: uses prune HH
doc ///
Key
  isAcyclic
  (isAcyclic, ChainComplex)
  (isAcyclic, ChainComplex, Ideal)
  (isAcyclic, ChainComplex, Module)
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///


-- NOT IMPLEMENTED compare isomorphism of the first module and equality of homologies
///
Key
  isHomotopic
  (isHomotopic, ChainComplex, ChainComplex)
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///


-- if matrix is over a local ring, return a matrix of number of terms in the numerator of each element
-- if matrix is over a poly ring, return a matrix of number of terms of each element
doc ///
Key
  sizeMatrix
  (sizeMatrix, Matrix)
  (sizeMatrix, MutableMatrix)
///
///
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///


end--


doc ///
Key
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///

