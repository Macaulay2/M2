-- Tests for the engine cone routines in Macaulay2/e/interface/cone.h:
--   rawFourierMotzkin, rawHilbertBasis, rawConeInteriorPoint,
--   rawLatticePoints, rawLatticePointsNormaliz, rawGVInvariants.

debug Core

------------------------------------------------------------
-- rawFourierMotzkin
-- Input: rows of A are inequalities of the cone {x : A x <= 0}.
-- Output: rows are the extreme rays of that cone.
------------------------------------------------------------
A = matrix {{1,1,1}, {-1,1,0}, {-1,0,1}, {1,2,3}}
B = map(ZZ, rawFourierMotzkin raw A)
assert(set entries B === set {{-1,-1,-1}, {1,-2,1}, {1,1,-2}})
-- Sanity: every ray r satisfies A r <= 0.
assert all(flatten entries (A * transpose B), x -> x <= 0)

------------------------------------------------------------
-- rawHilbertBasis
-- Input: rows of C are cone rays.
-- Output: rows are the Hilbert basis of that cone.
------------------------------------------------------------
C = matrix {{1,0}, {1,2}}
HB = map(ZZ, rawHilbertBasis raw C)
assert(set entries HB === set {{1,0}, {1,1}, {1,2}})

------------------------------------------------------------
-- rawConeInteriorPoint
-- Output is a 1-row matrix over RR(53):
--   full-dim:    [1, tStar, x_1, ..., x_n]
--   not full-dim: [0, tStar, y_1, ..., y_m]  with y >= 0 and y^T A = 0.
------------------------------------------------------------
-- Full-dimensional: first octant in QQ^3.
A = matrix {{-1,0,0}, {0,-1,0}, {0,0,-1}}
cipResult = flatten entries map(RR_53, rawConeInteriorPoint raw A)
assert(cipResult#0 == 1.0)
assert(cipResult#1 > 0)
interior = drop(cipResult, 2)
assert(#interior == 3)
-- Each row of A applied to the interior point should be strictly < 0.
assert all(flatten entries (A * transpose matrix {interior}), v -> v < 0)

-- Not full-dimensional: positive y-axis in QQ^2.
A = matrix {{1,0}, {-1,0}, {0,-1}}
cipResult = flatten entries map(RR_53, rawConeInteriorPoint raw A)
assert(cipResult#0 == 0.0)
y = drop(cipResult, 2)
assert(#y == 3)
assert all(y, v -> v >= 0)
-- y^T A = 0
assert all(flatten entries (matrix {y} * (A ** RR_53)), v -> abs v < 1e-8)

------------------------------------------------------------
-- rawLatticePoints
-- Enumerates integer x with A x <= b and |x_i| <= B.
------------------------------------------------------------
A = matrix {{1,1,1}}
b = matrix {{-2}}
LP = map(ZZ, rawLatticePoints(raw A, raw b, 1, 100, 1000))
assert(numrows LP == 3)
assert(numcols LP == 4)
assert(set entries transpose LP === set {{-1,-1,-1}, {0,-1,-1}, {-1,0,-1}, {-1,-1,0}})

------------------------------------------------------------
-- rawLatticePointsNormaliz
-- Enumerates ALL integer x with A x <= b (polyhedron must be bounded).
------------------------------------------------------------
A = matrix {{1,0}, {0,1}, {-1,0}, {0,-1}}
b = matrix {{1}, {1}, {0}, {0}}
LP = map(ZZ, rawLatticePointsNormaliz(raw A, raw b))
assert(numrows LP == 2)
assert(numcols LP == 4)
assert(set entries transpose LP === set {{0,0}, {0,1}, {1,0}, {1,1}})

------------------------------------------------------------
-- rawGVInvariants
-- Computes Gopakumar-Vafa invariants of a Calabi-Yau threefold.
-- M2_arrayint inputs use the length-prefix-per-row encoding.
------------------------------------------------------------
encodeArrayArrayInt = method()
encodeArrayArrayInt List := List => L -> (
    parts := flatten for i from 0 to #L - 1 list prepend(#L#i, L#i);
    prepend(#L, parts))

mori       = encodeArrayArrayInt {{-1,3,-1},{0,-2,1},{1,2,-1}}
lightcone  = encodeArrayArrayInt {}
gradingvec = {2, 4, 9}
Q          = encodeArrayArrayInt {{1,0,0,0,1,0,1},{0,1,0,1,0,1,1},
                                  {0,0,1,3,1,2,2}}
nefpart    = encodeArrayArrayInt {}
intnums    = encodeArrayArrayInt {{0,0,0,-1},{0,1,1,-2},{0,1,2,1},
                                  {1,1,1,8},{1,1,2,-4},{1,2,2,2},
                                  {2,2,2,-1}}
settings   = {6, 150, 0, 300000}

GV = transpose map(ZZ, rawGVInvariants(mori, lightcone, gradingvec, Q,
                                       nefpart, intnums, settings))
-- 19 (curve, GV) tuples; each row is [c_1, c_2, c_3, GV(c)].
assert(numcols GV == 4)
assert(numrows GV == 19)
-- Spot-check a few specific (curve, GV) pairs.
assert(member({1,0,0,126},   entries GV))
assert(member({0,1,0,580},   entries GV))
assert(member({1,1,0,20615}, entries GV))
