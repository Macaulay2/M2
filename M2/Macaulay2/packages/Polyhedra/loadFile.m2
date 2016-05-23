needsPackage "FourierMotzkin"

load "./exports.m2"

---------------------------------------------------------------
-- Sorting rays
---------------------------------------------------------------

-- A ray is a matrix ZZ^n <-- ZZ^1, so rays can be sorted by assembling them
-- into a matrix and calling "sortColumns".  We sort the rays, so that changes to 
-- the algorithm for computing the hash code of matrices doesn't affect what we do.

raySort = rays -> rays _ (reverse sortColumns (- matrix {rays}))

-- Methods declarations for methods defined for several objects, but not all of polyhedraHash
load "./methods.m2"

-- Objects
load "./polyhedraHash.m2"
   load "./polyhedralObject.m2"
      load "./cone.m2"
      load "./polyhedron.m2"
   load "./polyhedralObjectFamily.m2"
      load "./fan.m2"
      load "./polyhedralComplex.m2"

-- Method implementations
load "./intersection.m2"
load "./contains.m2"
load "./minkowskiSum.m2"
load "./standardConstructions.m2"

-- Legacy code
load "./legacy.m2"
load "./legacy_tests.m2"
load "./legacy_doc.m2"

end

-------------------------------------------------------------------------------
restart
loadPackage "Polyhedra"
check "Polyhedra"
