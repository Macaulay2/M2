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

-- Tests
load "./tests/core/cone_basics.m2"
load "./tests/core/polyhedron_basics.m2"
load "./tests/core/fan_basics.m2"
load "./tests/core/tests_from_polymake/fvector.m2"
load "./tests/core/tests_from_polymake/hilbert_basis.m2"
load "./tests/core/tests_from_polymake/lattice_points.m2"
load "./tests/core/tests_from_polymake/normal_fan.m2"
-- 
load "./tests/extended/polyhedron.m2"
load "./tests/extended/tests_from_polymake/minkowskiSum.m2"

-- Failing tests
-- load "./tests/failing.m2"
-- load "./tests/legacy_tests_failing.m2"

-- Documentation
-- load "./legacy_doc.m2"

end

-------------------------------------------------------------------------------
restart
loadPackage "Polyhedra"
check "Polyhedra"

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
verticesP = matrix {{0,-2/3,1/3,-1/3,1/3,-1/3,2/3},{0,-2/3,1/3,-1/3,1/3,-1/3,2/3},{0,1/3,-2/3,-1/3,1/3,2/3,-1/3},{0,1/3,-2/3,-1/3,1/3,2/3,-1/3},{0,1/3,1/3,2/3,1/3,2/3,2/3},{0,1/3,1/3,2/3,-2/3,-1/3,-1/3},{0,1/3,1/3,2/3,-2/3,-1/3,-1/3}};
raysP = matrix {{0,1,-1,1,0,0,1,1,0,1,1,1,0,0},{0,1,1/2,-2,0,0,1,1,1,0,1,1,0,0},{0,-2,1/2,1,0,1,1,1,0,0,1,1,0,0},{0,1,1/2,1,1,0,-2,1,0,0,1,1,0,0},{0,1,1/2,1,0,0,1,1,0,0,1,-2,1,0},{0,1,1/2,1,0,0,1,-2,0,0,1,1,0,1},{1,1,1/2,1,0,0,1,1,0,0,-2,1,0,0}};
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
raysdesired = matrix {{0,1,1,0,1,0,0,1,1,1,1,1,0,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0},{1,0,0,1,0,1,0,1,1,1,1,1,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,1,0,0,1},{0,0,0,0,0,0,0,1,0,0,0,0,1,1,1,0,0,1,1,1,1,0,1,1,0,0,1,1,1,0,0,1,1,0,0},{0,0,0,0,0,0,0,0,1,0,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,0,0,0,1,1,0,0,1,1},{0,0,1,1,1,1,1,0,0,1,0,0,1,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0},{1,1,0,0,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,1,1,1,1,0,0,0,0},{1,1,1,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,0,0,0,0,1,1,1,1}};
linealitydesired = map(QQ^7, QQ^0, 0);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
maxConesdesired = {{2,3,4,5,15,16,17,18,23,24,25,26,27,28,29,30,31,32,33,34},{2,3,4,5,7,8,9,10,11,15,16,17,18,27,28,29,30,31,32,33,34},{12,13,14,15,16,17,18,19,20,23,24,25,26,27,28,29,30,31,32,33,34},{7,8,13,14,15,16,17,18,27,28,29,30,31,32,33,34},{0,1,2,3,4,5,6,21,22,23,24,25,26,27,28,29,30,31,32,33,34},{0,1,2,3,4,5,10,11,27,28,29,30,31,32,33,34},{19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34}};
desired = fan (raysdesired ,linealitydesired ,maxConesdesired);
computed = normalFan P;
assert(computed == desired)

loadPackage "Polyhedra"
P = convexHull matrix {{0,-1,1,-1,1},{0,-1,-1,1,1},{-1,1,1,1,1}};
vertexEdgeMatrix P
M = matrix {{0,1,2,3,4,5,6,7,8},{1,1,1,0,1,1,0,0,0},{2,1,0,1,0,0,0,1,0},{3,0,0,0,1,0,1,1,0},{4,0,1,1,0,0,0,0,1},{5,0,0,0,0,1,1,0,1}};



P = convexHull matrix {{0,-1,1,0,0,1,-1},{0,0,0,1,-1,-1,1}};
P = bipyramid P;


restart
loadPackage "Polyhedra"
C = posHull matrix {{0},{1}}
hilbertBasis C

restart
loadPackage "Polyhedra"
P = convexHull matrix {{12,3},{3,5}}
faces P
C = getProperty(P, underlyingCone)
faces C
vertices P
getProperty(P, facetToFacetMap )


restart
loadPackage "Polyhedra"
P = convexHull matrix {{3,1,0,2},{0,2,2,1},{1,-1,2,0}};
assert(numColumns vertices P == 3)
assert(dim P == 2)
assert(ambDim P == 3)
assert(rays P == 0)
assert(linSpace P == 0)
M = matrix {{3,4,1}};
v = matrix {{10}};

restart
loadPackage "Polyhedra"
P = convexHull matrix {{3,1,0,2},{0,2,2,1},{1,-1,2,0}};
N = normalFan P
H = getProperty (N, computedFacesThroughRays )
smoothSubfan N

restart
loadPackage "Polyhedra"
M = transpose matrix {{1,2},{2,1},{1,0},{0,1}}
L = {{0,1},{1,2},{0,3}};
L2 = {{0,3},{1,2}}
F = fan(M, L);
assert not isSmooth F
F1 = fan(M, L2);
assert(smoothSubfan F == F1)


C = posHull matrix {{12,3},{3,5}}
facets C
hilbertBasis C
isPointed C
C = posHull matrix {{1,0,0},{0,-1,1}}
isPointed C



loadPackage "FourTiTwo"
linSpace C
rays C

f = (cacheValux {{12,3},{3,5}}e inputRays)(X->(<<"Hello."<<endl; 5))
C = new CacheTable from {x => 5}
X = new HashTable from {cache => C}
f X
Y = new HashTable from {cache => new CacheTable}
f Y 

hasProperty(C, computedDimension)
keys C
keys C.cache
hilbertBasis C
keys C.cache
dim C
keys C.cache
linSpace C
rays C
check "Polyhedra"


restart
loadPackage "Polyhedra"
raysC = matrix {{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
desiredHB = matrix {{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1},{-1,-1,-1,0,0,0,1,1,1,-1,-1,-1,0,0,0,1,1,1,-1,-1,-1,0,0,0,1,1,1,-1,-1,-1,0,0,0,1,1,1,-1,-1,-1,0,0,0,1,1,1,-1,-1,-1,0,0,0,1,1,1,-1,-1,-1,0,0,0,1,1,1,-1,-1,-1,0,0,0,1,1,1,-1,-1,-1,0,0,0,1,1,1},{-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1}};
desiredHB = sort desiredHB;
C = posHull(raysC)
computedHB = sort matrix {hilbertBasis C};

restart
loadPackage "PolyhedraOld"
verticesP = matrix {{0,0,1/2,0,1,1/2,0,0,0,0,0,1,0,0},{0,1,1/2,0,0,0,1/2,0,0,0,0,0,1,0},{1,0,1/2,0,0,0,0,1/2,0,0,0,0,0,1},{0,0,0,0,0,1/2,1/2,0,0,1,0,0,0,1},{0,0,0,0,0,1/2,0,1/2,1,0,0,0,1,0},{0,0,0,0,0,0,1/2,1/2,0,0,1,1,0,0}};
desiredLP = matrix {{0,0,0,0,0,0,0,0,1,1},{0,0,0,0,0,0,1,1,0,0},{0,0,0,0,1,1,0,0,0,0},{0,0,0,1,0,1,0,0,0,0},{0,0,1,0,0,0,0,1,0,0},{0,1,0,0,0,0,0,0,0,1}};
desiredLP = sort desiredLP;
P = convexHull(verticesP)
elapsedTime latticePoints P;

restart
loadPackage "Polyhedra"
verticesP = matrix {{0,0,1/2,0,1,1/2,0,0,0,0,0,1,0,0},{0,1,1/2,0,0,0,1/2,0,0,0,0,0,1,0},{1,0,1/2,0,0,0,0,1/2,0,0,0,0,0,1},{0,0,0,0,0,1/2,1/2,0,0,1,0,0,0,1},{0,0,0,0,0,1/2,0,1/2,1,0,0,0,1,0},{0,0,0,0,0,0,1/2,1/2,0,0,1,1,0,0}};
desiredLP = matrix {{0,0,0,0,0,0,0,0,1,1},{0,0,0,0,0,0,1,1,0,0},{0,0,0,0,1,1,0,0,0,0},{0,0,0,1,0,1,0,0,0,0},{0,0,1,0,0,0,0,1,0,0},{0,1,0,0,0,0,0,0,0,1}};
desiredLP = sort desiredLP;
P = convexHull(verticesP)
elapsedTime latticePoints P;
