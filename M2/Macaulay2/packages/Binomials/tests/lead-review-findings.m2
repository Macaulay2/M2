-- Regression tests for the findings of the review of the renewed package
-- (notes/review-renewed.md).  One section per finding.
debug Binomials;

-- B1. congruenceClasses computes one character per set of loop labels, not
-- one per connected component.  Components with different loop sets but the
-- same character form one class.
R = QQ[x,y];
I = ideal(y^3, y*(x^2 - 1), y^2*(x^4 - 1));
classes = congruenceClasses(I, {0});
assert(#classes == 2);
assert(classes#0#2 == {1_R} and classes#0#3 == 1);
assert(classes#1#0 == matrix {{2}} and classes#1#1 == {1} and classes#1#2 == {y, y^2} and classes#1#3 == 2);
-- 8000 components, all with the same character (14 s in the morning
-- version of 2.0, 2.3 s in 1.2.1 for n = 30; now 0.3 s)
R = QQ[x,y,z,w];
I = ideal(y^20, z^20, w^20, x^2 - 1);
classes = congruenceClasses(I, {0});
assert(#classes == 1);
assert(#classes#0#2 == 8000 and classes#0#3 == 8000);
assert(first classes#0#2 == 1_R);
aps = cellularBinomialAssociatedPrimes(I, CellVariables => {x});
assert(#aps == 2 and all({ideal(y, z, w, x - 1), ideal(y, z, w, x + 1)}, P -> any(aps, Q -> Q == P)));

-- B2. Exact rational roots of any size (1.2.1 and the morning version of
-- 2.0 used floating point and raised an error above about 2^53).
assert(rationalRoot((3^40 + 2)^3, 3) == 3^40 + 2);
assert(rationalRoot(10^400, 2) == 10^200);
assert(rationalRoot((7^200)^7, 7) == 7^200);
assert(rationalRoot((7^200)^7 + 1, 7) === null);
assert(rationalRoot(8/27, 3) == 2/3);
assert(rationalRoot(2, 2) === null);
assert(all(0 .. 300, n -> all(1 .. 5, d -> (
                r := rationalRoot(n, d);
                b := select(toList(0 .. n), t -> t^d == n);
                if #b == 0 then r === null else r == first b))));
R = QQ[x,y];
bigRoot = 3^40 + 2;
pd = binomialPrimaryDecomposition ideal(x^3 - bigRoot^3*y^3);
assert(#pd == 3 and any(pd, P -> P == sub(ideal(x - bigRoot*y), ring P)));
assert(intersect pd == sub(ideal(x^3 - bigRoot^3*y^3), ring first pd));
assert(set apply(binomialPrimaryDecomposition ideal(x^2 - (10^30 + 1)^2*y^2), P -> toString P)
    === set {toString ideal(x - (10^30 + 1)*y), toString ideal(x + (10^30 + 1)*y)});
assert(#binomialMinimalPrimes ideal(x^2 - 10^400*y^2) == 2);

-- B3. One field, one ring: N = 2 mod 4 is reduced to N/2, so the answer no
-- longer depends on the order of the cell variables or on how a root is
-- written.
R = QQ[x,y];
I = ideal(x - 1, y^3 + 1);
A1 = cellularBinomialAssociatedPrimes I;
A2 = cellularBinomialAssociatedPrimes(I, CellVariables => {y, x});
assert(ring first A1 === ring first A2);
assert(set apply(A1, toString) === set apply(A2, toString));
assert(ring first binomialPrimaryDecomposition ideal(y^3 + 1) === ring first binomialPrimaryDecomposition ideal(y^3 - 1));
assert(cyclotomicOrder coefficientRing ring first binomialMinimalPrimes ideal(x^10 - y^10) == 5);
assert(#binomialMinimalPrimes ideal(x^10 - y^10) == 10);
R = QQ[a,b,c];
I = ideal(c^2 + a, a*b - 1, a^4*c - b^3, a^5 + b^3*c);
A1 = cellularBinomialAssociatedPrimes I;
A2 = cellularBinomialAssociatedPrimes(I, CellVariables => {c, b, a});
assert(ring first A1 === ring first A2);
assert(cyclotomicOrder coefficientRing ring first A1 == 15);
assert(set apply(A1, toString) === set apply(A2, toString));

-- B4. Ideals that are not binomial are refused by every function that
-- needs a binomial ideal; 1.2.1 gave wrong answers (x^2+x+1 "prime").
R = QQ[x,y,z];
I = ideal(y*(x^2 - 1) + z*(x - 1), y^2, z^2, y*z);
assert(not isBinomial I);
scan({() -> binomialIsPrime ideal(x^2 + x + 1),
        () -> binomialIsPrimary ideal(x^2 + x + 1),
        () -> binomialRadical ideal((x^2 + x + 1)^2),
        () -> cellularBinomialAssociatedPrimes ideal((x^2 + x + 1)^2),
        () -> cellularBinomialAssociatedPrimes I,
        () -> cellularBinomialPrimaryDecomposition I,
        () -> cellularBinomialUnmixedDecomposition I,
        () -> cellularBinomialIsPrimary I,
        () -> cellularBinomialRadical(I, CellVariables => {x}),
        () -> partialCharacter ideal(x^2 + x + 1)},
    f -> assert(try (f(); false) else true));
-- binomial ideals with generators that are not binomials are accepted
J = ideal(x - y + z^2 - 1, x - y - z^2 + 1);
assert(binomialRadical J == J and not binomialIsPrime J);
A = cellularBinomialAssociatedPrimes(ideal(x^2 - y^2 + z, z), CellVariables => {x, y});
assert(#A == 2 and all({ideal(z, x - y), ideal(z, x + y)}, P -> any(A, Q -> Q == P)));

-- B7. No zero or duplicate generators in returned ideals (the morning
-- version of 2.0 returned ideal(x*y-1, 0); 1.2.1 and it returned
-- ideal(x, y, x, y)).
R = QQ[x,y];
noJunk = J -> all(J_*, g -> g != 0) and #unique J_* == #J_*;
assert(toString binomialRadical ideal(x*y - 1) == "ideal(x*y-1)");
assert(toString binomialRadical ideal(x, y) == "ideal(x,y)");
assert(toString binomialRadical ideal(x^2 - y^2) == "ideal(x^2-y^2)");
assert(binomialRadical ideal(x^2 - 1, y^2) == ideal(x^2 - 1, y));
assert(all(cellularBinomialPrimaryDecomposition ideal(x^4 - 1), noJunk));
assert(all(cellularBinomialIsPrimary(ideal(x - 1, y - 1), ReturnPrimes => true), noJunk));
assert(all(binomialMinimalPrimes ideal(x^3*y^2), noJunk));
assert(all(binomialAssociatedPrimes ideal(x^2*y, y^3, x*(y - 1)), noJunk));

-- B6, first part. In positive characteristic the points of binomialSolve
-- have coordinates in the coefficient field (they were in QQ).
R = ZZ/5[x,y];
s = binomialSolve ideal(x - 1, y^2 - y);
assert(ring first first s === ZZ/5 and set s === set {{1_(ZZ/5), 0_(ZZ/5)}, {1_(ZZ/5), 1_(ZZ/5)}});
F4 = GF 4;
R = F4[x,y];
s = binomialSolve ideal(x - 1, y^2 - y);
assert(ring first first s === F4 and #s == 2);
-- binomialSolve without the intersection of leaves: unchanged answers
R = QQ[x,y,z];
s = binomialSolve ideal(x^2 - 1, y^3 - 1, z^2);
assert(#s == degree ideal(x^2 - 1, y^3 - 1, z^2) and #unique s == 6);
assert(all(s, p -> p#0^2 == 1 and p#1^3 == 1 and p#2 == 0));
