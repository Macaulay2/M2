doc ///
Key
  PruneComplex
Headline
  Pruning chain complexes over polynomial and local rings
Description
  Text
    This package includes various methods for pruning chain complexes over polynomial and local rings.
    In particular, in the local or graded case the output is guaranteed to be a minimal free resolution.

    Algorithms in this package are also implemented using C++ in e/mutablecomplex.hpp for speed.
  Example
    R = ZZ/32003[vars(0..17)];
    m1 = genericMatrix(R,a,3,3)
    m2 = genericMatrix(R,j,3,3)
    I = ideal(m1*m2-m2*m1)
  Text
    Here we produce an intentionally nonminimal resolution:
  Example
    C = freeResolution(I, Strategy => Nonminimal)
  Text
    Now we prune the resolution above to get a minimal resolution:
  Example
    D = pruneComplex(C, UnitTest => isScalar)
    isCommutative D.cache.pruningMap
    betti D == betti freeResolution I
Caveat
  Only supports localization at prime ideals.
SeeAlso
  "LocalRings :: LocalRings"
///

doc ///
Key
   pruneComplex
  (pruneComplex, List)
  (pruneComplex, List, ZZ)
  (pruneComplex, Complex)
  (pruneComplex, Complex, ZZ)
Headline
  Prunes a chain complex or list of mutable matrices
Usage
  D = pruneComplex C
  D = pruneComplex(C, nsteps)
Inputs
  C: Complex
    or a list of mutable matrices defining the differentials of a complex
  nsteps: ZZ
    the number of maps in the complex that will be pruned
Outputs
  D: Complex
    or the list of modified mutable matrices
Description
  Text
    Prune a chain complex C by removing unit elements from the differentials.

    If C is a free resolution for an R-module M, the output will remain a free resolution of M.
    In particular, if M is homogeneous or R is a local ring, the output is guaranteed to be a minimal
    free resolution of M.
  Example
    R = ZZ/32003[a..f]
    I = ideal"abc-def,ab2-cd2-c,acd-b3-1"
  Text
    Here we produce an intentionally non-minimal resolution from a inhomogeneous ideal:
  Example
    S = (coefficientRing R)(monoid [gens R, local h]);
    Ihom = ideal homogenize(sub(gens gb I, S), S_(numgens R));
    Chom = (freeResolution(Ihom, Strategy => Nonminimal))[-10];
    C = (map(R, S, gens R | {1})) Chom
  Text
    Now we use pruneComplex to prune the resolution above:
  Example
    D = pruneComplex C
    D.dd
  Text
    One way of improving performance is to turn off computation of the pruning map. Note, however, that
    this may result in incorrect degrees in the graded case:
  Example
    D1 = pruneComplex(C, PruningMap => false)
    D1.dd
  Text
    Another method is to use a different pruning strategy. Note that in general there is no well-defined
    notion of minimality for chain complexes, so different strategies can lead to different results.
    See the page linked in the optional inputs section above for information on available strategies.

    As an example, we can alternate between pruning the lower and higher indices:
  Example
    D2 = pruneComplex(C, Strategy => null, Direction => "both")
    D2.dd
  Text
    For pruning chain complexes over local rings, pruning scalars by setting UnitTest => isScalar and
    then pruning other units using UnitTest => isUnit can improve speed. For homogeneous chain complexes
    (that is, when all maps are homogeneous), since all units are scalars, setting UnitTest => isScalar
    is always faster:
  Example
    R = ZZ/32003[vars(0..8)]
    M = genericMatrix(R,3,3)
    I = minors(2, M)
    C = freeResolution(I, Strategy => Nonminimal)
    pruneComplex(C, UnitTest => isScalar)
Consequences
  Item
    Unless PruningMap is false, D.cache.pruningMap will be updated to a chain complex map {f: C <-- D}.
Caveat
  For inhomogeneous input the resulting complex is not guaranteed to be minimal, particularly because
  minimality is not well-defined in that case.

  If PruningMap is false and the input is not the resolution of a graded ideal, the grading of the
  resulting complex may be incorrect.
SeeAlso
  pruneDiff
  pruningMap
///

doc ///
Key
   pruneDiff
  (pruneDiff, List, ZZ)
  (pruneDiff, List, ZZ, List)
  (pruneDiff, Complex, ZZ)
  (pruneDiff, Complex, ZZ, List)
Headline
  Prunes a single differential in a chain complex or list of mutable matrices
Usage
  D = pruneDiff(C, n)
  D = pruneDiff(C, n, M)
Inputs
  C: Complex
    or a list of mutable matrices defining the differentials of a complex
  n: ZZ
    the index of the differential in C
  M: List
    if provided, will initialize the map back to the original complex
Outputs
  D: Complex
    or the list of modified mutable matrices
  M: List
    unless PruningMap is false, will contain the map back to the original complex
Description
  Text
    Completely prunes the n-th differential of the chain complex C by removing unit elements.
  Text
    Computing the syzygy over a local ring using liftUp and pruneDiff:
  Example
    needsPackage "LocalRings";
    R = ZZ/32003[vars(0..5)];
    I = ideal"abc-def,ab2-cd2-c,-b3+acd";
    C = freeResolution I;
    M = ideal gens R;
    RM = localRing(R, M);
    F = C.dd_2;
    FM = F ** RM
  Text
    This is the process for finding the syzygy of FM:
  Example
    f = liftUp FM;
    g = syz f;
    h = syz g;
    C = {g ** RM, h ** RM}/mutableMatrix;
  Text
    Now we prune the map h, which is the first map from the right:
  Example
    C = pruneDiff(C, 1, PruningMap => false)
    g' = C#0;
  Text
    Scale each row with the common denominator of the corresponding column in FM:
  Example
    N = transpose entries FM;
    for i from 0 to numcols FM - 1 do
      rowMult(g', i, N_i/denominator//lcm);
  Text
    The syzygy of FM is:
  Example
    GM = map(source FM, , matrix g')
    kernel FM == image GM
Caveat
  This method does not use the algorithms in the engine, hence it might be slow.
SeeAlso
  pruneUnit
  pruneComplex
--  findSparseUnit
///

doc ///
Key
   pruneUnit
  (pruneUnit, List, ZZ, Sequence, List)
Headline
  Prunes a unit of a differential in a list of mutable matrices
Usage
  D = pruneUnit(C, n, u, M)
Inputs
  C: List
    a list of mutable matrices defining the differentials of a complex
  n: ZZ
    the index of the differential in C
  u: Sequence
    the coordinates of the unit in the differential
  M: List
    if provided, will initialize the list of map back to the original complex
Outputs
  D: List
    a list of modified mutable matrices
Description
  Text
    Prune the unit in location {u} of the {n}-th differential in chain complex {C}.
    Moves the unit to the end, clears row and column, while keeping the neighboring differentials compatible.
Caveat
  Currently only works with 1x1 units.
SeeAlso
  pruningMap
  pruneDiff
///

doc ///
Key
   toChainComplex
  (toChainComplex, List)
  (toChainComplex, List, Module)
Headline
  Converts a list of mutable matrices into a ChainComplex.
Usage
  C = toChainComplex(M, F)
Inputs
  M: List
    a list of MutableMatrix type
  F: Module
    the initial free module in the resolution. This is used to calculate the degrees in the maps.
Outputs
  C: Complex
Description
  Example
    R = ZZ/32003[vars(0..17)];
    m1 = genericMatrix(R,a,3,3)
    m2 = genericMatrix(R,j,3,3)
    I = ideal(m1*m2-m2*m1)
    C = freeResolution I;
    D = C[-10]
    MC = toMutableComplex D;
    MC = first pruneComplex MC;
    D' = toChainComplex MC
    assert(betti D == betti D'[-10])
Caveat
  Since the information about source of target of maps is not available, the grading may be incorrect
  for general complexes.
SeeAlso
  toMutableComplex
///

doc ///
Key
   toMutableComplex
  (toMutableComplex, Complex)
Headline
  Converts a chain complex into a list of mutable matrices.
Usage
  M = toMutableComplex C
Inputs
  C: Complex
Outputs
  M: List
    a list of MutableMatrix type
Description
  Example
    needsPackage "LocalRings"
    R = ZZ/32003[vars(0..3)]
    I = monomialCurveIdeal(R, {1, 3, 4})
    C = freeResolution I
    RP = localRing(R, ideal"a,b,c");
    D = (C ++ C[-5]) ** RP
    MD = toMutableComplex D
    pruneComplex MD
Caveat
  The nonzero terms in the chain complex must be in a series, otherwise may not work correctly.
SeeAlso
  toChainComplex
///

///
Key
   findUnit
  (findUnit, Matrix)
  (findUnit, MutableMatrix)
  (findUnit, Matrix, Sequence)
  (findUnit, MutableMatrix, Sequence)
///

///
Key
   findAllUnits
  (findAllUnits, Matrix)
  (findAllUnits, MutableMatrix)
///

-- option: Strategy: NoSort
///
Key
   findSparseUnit
  (findSparseUnit, Matrix)
  (findSparseUnit, MutableMatrix)
///

-- uses liftable(f, coefficientRing ring f)
doc ///
Key
  isScalar
  (isScalar, RingElement)
Headline
  check whether a ring element is a scalar
Usage
  b = isScalar(f)
Inputs
  f: RingElement
    a ring element
Outputs
  b: Boolean
    true if f is a scalar, false otherwise.
Description
  Text
    A synonym for liftable(f, coefficientRing ring f).
SeeAlso
  isUnit
  UnitTest
  liftable
///

doc ///
Key
  PruningMap
  [pruneUnit, PruningMap]
  [pruneDiff, PruningMap]
  [pruneComplex, PruningMap]
Headline
  Whether to compute a morphism of complexes
Description
  Text
    When true, the pruning map to the original complex will be stored in C.cache.pruningMap.
Caveat
  Setting PruningMap to false will improve performance, but in some cases the grading of the
  resulting object may be incorrect.
SeeAlso
  pruningMap
  pruneUnit
  pruneDiff
  pruneComplex
///

doc ///
Key
  UnitTest
  [pruneUnit, UnitTest]
  [pruneDiff, UnitTest]
  [pruneComplex, UnitTest]
Headline
  Limit which units are to be pruned
Description
  Text
    Possible values: isUnit, isScalar
SeeAlso
  pruneComplex
  pruneDiff
  pruneUnit
  isScalar
  isUnit
///

doc ///
Key
  Direction
  [pruneComplex, Direction]
Headline
  Determines the direction with which the matrices in the complex is pruned
Description
  Text
    Possible values:

    "left": Prune the differentials from lower indices.

    "right": Prune the differentials from higher indices.

    "both": Alternate between pruning the lower and higher indices.

    "best": Use heuristics to prune the best unit in the whole complex first.
SeeAlso
  pruneComplex
///

doc ///
Key
  [pruneComplex, Strategy]
Headline
  Whether to use the methods in the package or C++ algorithms in the engine
Description
  Text
    Possible values:

      null: use the algorithms written using the Macaulay2 language;

      Engine: use the algorithms implemented using C++ in the Engine (version 1.11 and up).

    For optional inputs, the Engine algorithms only support PruningMap, "left" or "right" as Direction,
    and isUnit or isScalar as UnitTest.

    Advanced users can implement their own strategies in packages/PruneComplex.m2 and run pruneComplex
    using the Strategy => null option.
SeeAlso
  pruneComplex
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
