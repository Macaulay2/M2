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

-- Methods declarations for methods defined for several objects, but not all of polyhedraHash
load "./methods.m2"
load "./faceBuilders.m2"

-- Objects
load "./objects/polyhedraHash.m2"
   load "./objects/polyhedralObject.m2"
      load "./objects/cone/constructors.m2"
      load "./objects/cone/properties.m2"
      load "./objects/cone/methods.m2"

--         load "./objects/cone/hilbertBasis.m2"
      load "./objects/polyhedron/constructors.m2"
      load "./objects/polyhedron/properties.m2"
      load "./objects/polyhedron/methods.m2"
      load "./objects/polyhedron.m2"
   load "./objects/polyhedralObjectFamily.m2"
      load "./objects/fan/constructors.m2"
      load "./objects/fan.m2"
      load "./objects/polyhedralComplex.m2"

-- Method not associated with fixed object type
load "./intersection.m2"
load "./directProduct.m2"
load "./contains.m2"
load "./minkowskiSum.m2"
load "./standardConstructions.m2"
load "./commonFace.m2"
load "./incompare.m2"
load "./affineImages.m2"

-- Legacy code
load "./legacy.m2"

-- Tests
load "./tests/cone_basics.m2"
load "./tests/polyhedron_basics.m2"
load "./tests_from_polymake/hilbert_basis.m2"
-- Failing tests
load "./tests/failing.m2"
-- load "./tests_from_polymake/lattice_points.m2"
-- load "./legacy_tests.m2"

-- Documentation
-- load "./legacy_doc.m2"

end

-------------------------------------------------------------------------------
restart
loadPackage "Polyhedra"
P = convexHull matrix {{12,3},{3,5}}
C = getProperty(P, underlyingCone)
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
