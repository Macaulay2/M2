needsPackage "FourierMotzkin"
needsPackage "FourTiTwo"

load "./exports.m2"
load "./symbols.m2"

---------------------------------------------------------------
-- Sorting rays
---------------------------------------------------------------

-- A ray is a matrix ZZ^n <-- ZZ^1, so rays can be sorted by assembling them
-- into a matrix and calling "sortColumns".  We sort the rays, so that changes to 
-- the algorithm for computing the hash code of matrices doesn't affect what we do.

raySort = rays -> rays _ (reverse sortColumns (- matrix {rays}))

-- Helper methods
load "./helpers.m2"

-- Methods declarations for methods defined for several objects, but not all of polyhedraHash
load "./declarations.m2"

-- Core
load "./core/polyhedraHash.m2"
   load "./core/polyhedralObject.m2"
      load "./core/cone/constructors.m2"
      load "./core/cone/properties.m2"
      load "./core/cone/methods.m2"
      load "./core/polyhedron/constructors.m2"
      load "./core/polyhedron/properties.m2"
      load "./core/polyhedron/methods.m2"
      load "./core/polyhedron.m2"
   load "./core/polyhedralObjectFamily.m2"
      load "./core/fan/constructors.m2"
      load "./core/fan/methods.m2"
      load "./core/fan/properties.m2"
      load "./core/fan.m2"
      load "./core/polyhedralComplex.m2"
load "./core/contains.m2"
load "./core/intersection.m2"

-- Extended
load "./extended/directProduct.m2"
load "./extended/minkowskiSum.m2"
load "./extended/polyhedron/properties.m2"
load "./extended/polyhedron/methods.m2"

load "./standardConstructions.m2"
load "./commonFace.m2"
load "./incompare.m2"
load "./affineImages.m2"

-- Legacy code
load "./legacy.m2"
load "./not_refactored.m2"

-- Tests
load "./tests/core/cone_basics.m2"
load "./tests/core/polyhedron_basics.m2"
load "./tests/core/fan_basics.m2"
load "./tests/core/tests_from_polymake/fvector.m2"
load "./tests/core/tests_from_polymake/representation.m2"
load "./tests/core/tests_from_polymake/hilbert_basis.m2"
load "./tests/core/tests_from_polymake/lattice_points.m2"
load "./tests/core/tests_from_polymake/normal_fan.m2"
 
load "./tests/extended/polyhedron.m2"
load "./tests/extended/tests_from_polymake/minkowskiSum.m2"

load "./tests/legacy_tests_working.m2"

-- Failing tests
-- load "./tests/failing.m2"

-- Documentation
-- load "./legacy_doc.m2"

end

-------------------------------------------------------------------------------
restart
loadPackage "Polyhedra"
check "Polyhedra"

restart
loadPackage "Polyhedra"
verticesP = map(QQ^0, QQ^1, 0);
raysP = map(QQ^0, QQ^0, 0);
linealityP = map(QQ^0, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = map(QQ^1, QQ^0, 0);
ineqrhsPd = matrix {{1}};
eqlhsPd = map(QQ^0, QQ^0, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)


restart
loadPackage "Polyhedra"
M = matrix {{1,1,1,1,-1,-1,-1,-1},{1,1,-1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1,1,-1}};
P = convexHull M
assert(ring getProperty(P, points) === QQ)
v = matrix {{1},{1},{1}};
I = (M,v)
P = intersection I
l = getProperty(P,inequalities)
assert(ring l#0 === QQ)
assert(ring l#1 === QQ)

loadPackage "PolyhedraOld"
P1 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{1,1,-1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1,1,-1}};
P2 = intersection(matrix {{1,0,0},{-1,0,0}},matrix {{-1},{-1}});
assert isFace(P2,P1)


restart
loadPackage "Polyhedra"
verticesP = matrix {{0,0,1/2,0,1,1/2,0,0,0,0,0,1,0,0},{0,1,1/2,0,0,0,1/2,0,0,0,0,0,1,0},{1,0,1/2,0,0,0,0,1/2,0,0,0,0,0,1},{0,0,0,0,0,1/2,1/2,0,0,1,0,0,0,1},{0,0,0,0,0,1/2,0,1/2,1,0,0,0,1,0},{0,0,0,0,0,0,1/2,1/2,0,0,1,1,0,0}};
desiredLP = matrix {{0,0,0,0,0,0,0,0,1,1},{0,0,0,0,0,0,1,1,0,0},{0,0,0,0,1,1,0,0,0,0},{0,0,0,1,0,1,0,0,0,0},{0,0,1,0,0,0,0,1,0,0},{0,1,0,0,0,0,0,0,0,1}};
desiredLP = sort desiredLP;
P = convexHull(verticesP)
elapsedTime latticePoints P;
