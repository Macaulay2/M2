-- doc-unmixed-components.m2
-- binomialUnmixedDecomposition and cellularBinomialUnmixedDecomposition
-- return components without embedded primes.
-- Documented: "An ideal is unmixed if all its associated primes have the
-- same dimension."
-- QQ[x,y,z]: ideal(x^2,x*y,y^2,x*(z^3-1),y*(z^2-1)) has the associated primes
--   (y,x), (z-1,y,x), (z+1,y,x), (y,x,z^2+z+1).
-- QQ[x,y]: ideal(x*(y^3-1),x^2), CellVariables=>{y}, has the associated
--   primes (x), (y-1,x), (x,y^2+y+1).
-- Also ideal(-y^2*z^2+x*y,-y^3*z^2+x*y,x*y*z^3).
-- Expected: every component has all associated primes of the same dimension.

isUnmixed = J -> (d := dim J; all(associatedPrimes J, P -> dim P == d));
R = QQ[x,y,z];
assert(all(binomialUnmixedDecomposition ideal(x^2, x*y, y^2, x*(z^3-1), y*(z^2-1)), isUnmixed));
assert(all(binomialUnmixedDecomposition ideal(-y^2*z^2+x*y, -y^3*z^2+x*y, x*y*z^3), isUnmixed));
R = QQ[x,y];
assert(all(cellularBinomialUnmixedDecomposition(ideal(x*(y^3-1), x^2), CellVariables => {y}), isUnmixed));
