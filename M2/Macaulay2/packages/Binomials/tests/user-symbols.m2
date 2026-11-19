-- Results must not depend on the values of the symbols that name the ring
-- variables, and the package must not create symbols for the user.
R = QQ[x,y,z];
I = ideal(x^2, x*y - x, z^2);
before = binomialAssociatedPrimes I;
assert(#before == 2);
x = z;                         -- the user reuses the name x; R is unchanged
after = binomialAssociatedPrimes I;
assert(#after == 2 and all(2, i -> before#i == after#i));
x = 5;                         -- x is a number now; R is unchanged
J = ideal(R_0^2 * (R_1 - 1), R_0^3);
bpd = binomialPrimaryDecomposition J;
assert(intersect bpd == J and all(bpd, isPrimary) and #bpd == 2);
-- auxiliary variables of the package are not the user's
m0 = 7;
L = binomialPrimaryDecomposition ideal(R_0^2 - R_1^2);
assert(#L == 2 and intersect L == ideal(R_0^2 - R_1^2));
assert(m0 === 7);
-- a ring that was never assigned to its symbols
f = () -> (S := QQ(monoid [getSymbol "a", getSymbol "b", getSymbol "c"]); ideal(S_0^2 * (S_1^3 - 1), S_0^3, S_2 - S_1));
K = f();
bpd = binomialPrimaryDecomposition K;
assert(#bpd == 4 and intersect bpd == sub(K, ring first bpd));
-- the user's variables are not reassigned by a field extension
use R;
x = R_0;
binomialPrimaryDecomposition ideal(R_0^3 - R_1^3);
assert(ring x === R);
