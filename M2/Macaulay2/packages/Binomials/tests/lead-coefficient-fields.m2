-- Coefficient fields and rings: what is supported, what is refused, and in
-- which ring results live.
needsPackage "Cyclotomic";
-- 1. the extension ring keeps the monomial order and the degrees of the input ring
R = QQ[x,y, MonomialOrder => Lex, Degrees => {2,3}];
L = binomialPrimaryDecomposition ideal(x^3 - y^3);
S = ring first L;
assert(#L == 3 and coefficientRing S =!= QQ);
assert((options S).MonomialOrder === (options R).MonomialOrder);
assert(degrees S == degrees R);
use R;                           -- assigning S to a symbol made x and y variables of S
-- 2. one extension ring per ring: results of different calls can be compared
M = binomialMinimalPrimes ideal(x^3 - y^3);
assert(ring first M === S);
assert(all(L, P -> any(M, Q -> P == Q)));
-- 3. no extension, no new ring
assert(ring first binomialPrimaryDecomposition ideal(x^2 - y^2) === R);
-- 4. input over a cyclotomic field: the result ring contains that field
F = cyclotomicField 4;
T = F[u,v];
L = binomialPrimaryDecomposition ideal(u^3 - v^3);
assert(#L == 3);
assert(intersect L == sub(ideal(u^3 - v^3), ring first L));
i = F_0;
I4 = ideal(u^2 - i * v^2);                                 -- needs a square root of i
L = binomialPrimaryDecomposition I4;
T8 = ring first L;
w = (coefficientRing T8)_0;                                -- a primitive 8th root of unity, i = w^2
assert(#L == 2 and intersect L == (map(T8, T, {T8_0, T8_1, w^2})) I4);
use T;
L = binomialPrimaryDecomposition ideal(u^2 + v^2);         -- splits over F already
assert(#L == 2 and ring first L === T);
-- 5. a number field that is not cyclotomic: version 1.2.1 did not terminate
K = toField(QQ[a]/(a^2 - 2));
U = K[x,y];
L = binomialPrimaryDecomposition ideal(x^2 - y^2);
assert(#L == 2 and ring first L === U and intersect L == ideal(x^2 - y^2));
assert(try (binomialPrimaryDecomposition ideal(x^3 - y^3); false) else true);   -- would need cube roots of unity
-- 6. positive characteristic: fine as long as all lattices are saturated
P = ZZ/101[x,y,z];
I = ideal(x*y - z, x*z - y^2);
L = binomialPrimaryDecomposition I;
assert(intersect L == I and all(L, isPrimary) and #L == #associatedPrimes I);
assert(binomialIsPrime ideal(x - y) and not binomialIsPrime ideal(x^2 - y^2, z));
C = binomialCellularDecomposition ideal(x^2 * y - y^3, z^2);
assert(intersect C == ideal(x^2 * y - y^3, z^2));
-- ... and since version 2.0 also otherwise (tests pchar-*.m2): x^2 - 1 is
-- primary in characteristic 2, with radical x - 1
P2 = ZZ/2[x];
assert(binomialPrimaryDecomposition ideal(x^2 - 1) == {ideal(x^2 - 1)});
assert(binomialIsPrimary ideal(x^2 - 1));
assert(binomialRadical ideal(x^2 - 1) == ideal(x - 1));
-- 7. rings that are refused
A = QQ[x,y] / ideal(x^2 - y^2);
assert(try (binomialPrimaryDecomposition ideal(x*y - 1); false) else true);
B = QQ[z,y,x, MonomialOrder => RevLex, Global => false];
assert(try (binomialPrimaryDecomposition ideal(x*y - z); false) else true);
C = ZZ[x,y];
assert(try (binomialPrimaryDecomposition ideal(x*y - 1); false) else true);
