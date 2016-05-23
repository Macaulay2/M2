needsPackage "FourierMotzkin"

load "./exports.m2"

---------------------------------------------------------------
-- Sorting rays
---------------------------------------------------------------

-- A ray is a matrix ZZ^n <-- ZZ^1, so rays can be sorted by assembling them
-- into a matrix and calling "sortColumns".  We sort the rays, so that changes to 
-- the algorithm for computing the hash code of matrices doesn't affect what we do.

raySort = rays -> rays _ (reverse sortColumns (- matrix {rays}))

load "./polyhedralObject.m2"
load "./cone.m2"
load "./legacy.m2"
load "./legacy_tests.m2"
load "./legacy_doc.m2"
