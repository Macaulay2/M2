needsPackage "FourierMotzkin"

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
load "./primitiveRules.m2"
load "./faceBuilders.m2"

-- Objects
load "./objects/polyhedraHash.m2"
   load "./objects/polyhedralObject.m2"
      load "./objects/cone.m2"
         load "./objects/cone/hilbertBasis.m2"
      load "./objects/polyhedron.m2"
   load "./objects/polyhedralObjectFamily.m2"
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
load "./tests_from_polymake/hilbert_basis.m2"
-- load "./tests_from_polymake/lattice_points.m2"
-- load "./legacy_tests.m2"

-- Documentation
load "./legacy_doc.m2"

end

-------------------------------------------------------------------------------
restart
loadPackage "Polyhedra"
C = posHull matrix {{12,3},{3,5}}
hilbertBasis C
dim C
linSpace C
rays C
check "Polehedra"
