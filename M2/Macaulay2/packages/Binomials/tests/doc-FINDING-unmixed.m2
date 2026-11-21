-- doc-FINDING-unmixed.m2
-- FINDING (or documentation error): binomialUnmixedDecomposition and
-- cellularBinomialUnmixedDecomposition return components with embedded
-- primes, already on their documented examples.
-- Documentation: "a list of unmixed components of I"; Caveat: "Apart from
-- unmixedness, properties of the output decomposition are defined only by the
-- course of the algorithm".  Unmixed = no embedded associated primes (all
-- associated primes of the same dimension).
-- Observed (QQ[x,y,z]): BUD ideal(x^2,x*y,y^2,x*(z^3-1),y*(z^2-1))
--   = {ideal(y^2,x*y,x^2,y*z^2-y,x*z^3-x)}, whose associated primes are
--   {ideal(y,x), ideal(z-1,y,x), ideal(z+1,y,x), ideal(y,x,z^2+z+1)}.
-- Observed (QQ[x,y]): cellularBinomialUnmixedDecomposition(ideal(x*(y^3-1),x^2),
--   CellVariables=>{y}) = {ideal(x*y^3-x,x^2)}, associated primes
--   {ideal x, ideal(y-1,x), ideal(x,y^2+y+1)}.
-- Also ideal(-y^2*z^2+x*y,-y^3*z^2+x*y,x*y*z^3): BUD component
--   ideal(z^2-x,x*y-x,x^2*z,x^3) has associated primes (z,x) and (z,y-1,x).
-- Possibly "unmixed" is meant in the sense of [OS00] for cellular ideals; the
-- documentation does not define it.  Expected (standard meaning): every
-- component has all associated primes of the same dimension.

isUnmixed = J -> (d := dim J; all(associatedPrimes J, P -> dim P == d));
R = QQ[x,y,z];
assert(all(binomialUnmixedDecomposition ideal(x^2, x*y, y^2, x*(z^3-1), y*(z^2-1)), isUnmixed));
assert(all(binomialUnmixedDecomposition ideal(-y^2*z^2+x*y, -y^3*z^2+x*y, x*y*z^3), isUnmixed));
R = QQ[x,y];
assert(all(cellularBinomialUnmixedDecomposition(ideal(x*(y^3-1), x^2), CellVariables => {y}), isUnmixed));
