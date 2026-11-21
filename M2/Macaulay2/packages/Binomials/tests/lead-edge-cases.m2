-- Corner cases: zeroIdeal and unit ideal, monomial ideals, ideals given by
-- generators that are not binomials, unusual variable names, error messages.
load (Binomials#"auxiliary files" | "tests/helpers/oracles.m2");
R = QQ[x,y,z];
zeroIdeal = ideal 0_R;
unitIdeal = ideal 1_R;
-- the zero ideal is prime, cellular with all variables as cell variables
assert(binomialPrimaryDecomposition zeroIdeal == {zeroIdeal} and binomialMinimalPrimes zeroIdeal == {zeroIdeal});
assert(binomialAssociatedPrimes zeroIdeal == {zeroIdeal} and binomialCellularDecomposition zeroIdeal == {zeroIdeal});
assert(binomialUnmixedDecomposition zeroIdeal == {zeroIdeal} and binomialRadical zeroIdeal == zeroIdeal);
assert(binomialIsPrime zeroIdeal and binomialIsPrimary zeroIdeal and isCellular zeroIdeal and isBinomial zeroIdeal and isUnital zeroIdeal);
assert(isCellular(zeroIdeal, ReturnCellVars => true) == {x, y, z});
-- the unit ideal has no components, no primes and no points, and is neither prime nor primary
assert(binomialPrimaryDecomposition unitIdeal == {} and binomialMinimalPrimes unitIdeal == {} and binomialAssociatedPrimes unitIdeal == {});
assert(binomialCellularDecomposition unitIdeal == {} and binomialUnmixedDecomposition unitIdeal == {} and binomialSolve unitIdeal == {});
assert(cellularBinomialAssociatedPrimes unitIdeal == {} and cellularBinomialPrimaryDecomposition unitIdeal == {} and cellularBinomialUnmixedDecomposition unitIdeal == {});
assert(not binomialIsPrime unitIdeal and not binomialIsPrimary unitIdeal and not cellularBinomialIsPrimary unitIdeal);
assert(binomialRadical unitIdeal == unitIdeal);
assert(binomialPrimaryDecomposition ideal(x - 1, x^2) == {});
-- monomial ideals
I = ideal(x^2*y, y^2*z);
assert orcCheckPrimaryDecomposition(I, binomialPrimaryDecomposition I);
assert orcCheckAssociatedPrimes(I, binomialAssociatedPrimes I);
assert orcCheckMinimalPrimes(I, binomialMinimalPrimes I);
assert(binomialRadical I == ideal(x*y, y*z));
-- a binomial ideal given by generators that are not binomials
J = ideal(x - y + z^2 - 1, x - y - z^2 + 1);
assert(isBinomial J and J == ideal(x - y, z^2 - 1));
assert orcCheckPrimaryDecomposition(J, binomialPrimaryDecomposition J);
assert orcCheckAssociatedPrimes(J, binomialAssociatedPrimes J);
-- input that is not binomial is refused
N = ideal(x + y + z);
assert(not isBinomial N);
scan({binomialPrimaryDecomposition, binomialMinimalPrimes, binomialAssociatedPrimes, binomialUnmixedDecomposition},
    f -> assert(try (f N; false) else true));
-- wrong cell variables are detected instead of looping
assert(try (cellularBinomialAssociatedPrimes(ideal(x*y - y), CellVariables => {x}); false) else true);
-- unitIdeal variable
S = QQ[t];
assert(#binomialPrimaryDecomposition ideal(t^6 - 1) == 6);
assert(#binomialPrimaryDecomposition ideal(t^5 - t^3) == 3);
assert orcCheckPrimaryDecomposition(ideal(t^5 - t^3), binomialPrimaryDecomposition ideal(t^5 - t^3));
-- indexed variables, and a variable that is called like the root of unity
T = QQ[u_1..u_3, ww];
K = ideal(u_1^3 - u_2^3, u_3^2, u_3*(ww^2 - 1));
assert orcCheckPrimaryDecomposition(K, binomialPrimaryDecomposition K);
assert orcCheckAssociatedPrimes(K, binomialAssociatedPrimes K);
-- many variables, few used
U = QQ[a_1..a_12];
K = ideal(a_3^2 - a_7^2, a_11^3);
bpd = binomialPrimaryDecomposition K;
assert(#bpd == 2 and intersect bpd == K and all(bpd, isPrimary));
-- cell variables may be given in any order and from another ring with the same variables
use R;
I = ideal(x^2, x*(y^3 - 1), z - y);
ap1 = cellularBinomialAssociatedPrimes(I, CellVariables => {y, z});
ap2 = cellularBinomialAssociatedPrimes(I, CellVariables => {z, y});
assert(#ap1 == 4 and all(4, i -> ap1#i == ap2#i));
R' = QQ[x,y,z];
ap3 = cellularBinomialAssociatedPrimes(I, CellVariables => {R'_1, R'_2});
assert(#ap3 == 4 and ring first ap3 === ring first ap1);
