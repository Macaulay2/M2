needsPackage "FourierMotzkin"
-- needsPackage "FourTiTwo"

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
      load "./core/polyhedralComplex/constructors.m2"
      load "./core/polyhedralComplex/properties.m2"
      load "./core/polyhedralComplex/methods.m2"
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
load "./legacy_doc.m2"

end

-------------------------------------------------------------------------------
restart
loadPackage "Polyhedra"
check "Polyhedra"

restart
loadPackage "Polyhedra"
P = hypercube 3
f = first faces(1,P)
f = convexHull (vertices P)_(f#0)
fv = polarFace f

restart
loadPackage "Polyhedra"
C = posHull matrix {{1,0,0},{0,1,1},{0,0,1}};
F = fan C
C = posHull matrix {{-1,0,0},{0,1,0},{0,0,1}};
incompCones(C,F)

restart
loadPackage "Polyhedra"
C2 = posHull matrix {{-1,0,0},{0,1,0},{0,0,1}};
C3 = posHull matrix {{-1,0,0},{0,1,0},{0,0,-1}};
C4 = posHull matrix {{-1,0,0},{0,-1,0},{0,0,1}};
C5 = posHull matrix {{-1,0,0},{0,-1,0},{0,0,-1}};
F1 = fan {C2,C3,C4,C5}
C6 = posHull matrix {{1,0,0},{0,-1,0},{0,0,1}};
C7 = posHull matrix {{1,0,0},{0,-1,0},{0,0,-1}};
F1 = addCone( {C6,C7}, F1)
F3 = fan {posHull matrix {{1}},posHull matrix {{-1}}}
F1 = F3 * F1


restart
loadPackage "Polyhedra"
R = matrix {{1,1,2},{2,1,1}}
C = posHull R
HS = halfspaces C
R1 = R || matrix {{0,0,0}}
LS = matrix {{1},{1},{1}}
C1 = posHull(R1,LS)
HS = transpose R1
hyperplanesTmp = matrix {{1,1,1}}
C2 = intersection(HS,hyperplanesTmp)
C3 = intersection HS
C4 = posOrthant 3
C5 = intersection(C1,C2)
C6 = posHull(C1,C2)
R2 = matrix {{2,-1},{-1,2},{-1,-1}}
C7 = posHull {R2,C3,C4}
P = crossPolytope 3
P1 = C6 + P
C8 = C * C1

p = hypercube 3
pc = polyhedralComplex p

restart
loadPackage "Polyhedra"
uninstallPackage "Polyhedra"
installPackage "Polyhedra"


M1 = matrix{{0,0,0,0,0},{1,1,0,0,0},{1,2,0,0,0}}
P1 = convexHull transpose M1
M2 = matrix{{0,0,0,0,0},{1,0,1,0,0},{1,0,2,0,0}}
P2 = convexHull transpose M2
M3 = matrix{{0,0,0,0,0},{1,0,0,1,0},{1,0,0,2,0}}
P3 = convexHull transpose M3
M4 = matrix{{0,0,0,0,0},{1,0,0,0,1},{1,0,0,0,2}}
P4 = convexHull transpose M4
M5 = matrix{{0,1,1,0,0},{0,0,0,1,1}}
P5 = convexHull transpose M5
mixedVolume({P1,P2, P3,P4, P5})


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
