-- Cellular ideals with more than 100000 standard monomials in their
-- nilpotent variables.  Macaulay2 refuses ranges a ..< b with more than
-- 100000 elements ("ZZ .. ZZ: very long sequence requested"), and
-- congruenceClasses used one.  Found with the adjacent 2x2 minors of a 4x5
-- matrix, whose largest cellular component has 450149 such monomials
-- (LOG.md, session 4).
debug Binomials;

-- 7^6 = 117649 standard monomials in x_1..x_6.  The monomials divisible by
-- x_1^6 have the lattice ZZ with character 1 (t x_1^6 = x_1^6), all others
-- the zero lattice.
R = QQ[x_1..x_6, t];
I = ideal(apply(6, i -> x_(i+1)^7)) + ideal(x_1^6*(t - 1));
classes = congruenceClasses(I, {6});
assert(#classes == 2);
assert(sum(classes, cl -> #cl#2) == 7^6);
assert(numcols classes#0#0 == 0 and #classes#0#2 == 7^6 - 7^5 and classes#0#3 == 7^6 - 7^5);
assert(classes#1#0 == matrix {{1}} and classes#1#1 == {1} and #classes#1#2 == 7^5 and classes#1#3 == 7^5);
assert(first classes#0#2 == 1_R and first classes#1#2 == x_1^6);
-- the monomials are in the order of the earlier code: by degree, each
-- degree in descending lexicographic order of the exponents
V = classes#0#2;
assert(take(V, 9) == {1_R, x_1, x_2, x_3, x_4, x_5, x_6, x_1^2, x_1*x_2});
assert(apply(V, m -> first degree m) == sort apply(V, m -> first degree m));
assert(apply(select(V, m -> first degree m == 2), m -> (first exponents m)_{0..5})
    == rsort apply(select(V, m -> first degree m == 2), m -> (first exponents m)_{0..5}));
aps = cellularBinomialAssociatedPrimes(I, CellVariables => {t});
assert(#aps == 2);
assert(aps#0 == ideal(x_1, x_2, x_3, x_4, x_5, x_6));
assert(aps#1 == ideal(x_1, x_2, x_3, x_4, x_5, x_6, t - 1));
pd = cellularBinomialPrimaryDecomposition(I, CellVariables => {t});
assert(#pd == 2);
assert(pd#0 == I + ideal(x_1^6));
assert(pd#1 == I + ideal(t - 1));
assert(intersect pd == I);
