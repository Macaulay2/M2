-- Amplitude homology uses incoming and outgoing products with complementary
-- lengths. Distinct ranks ensure their ambient modules cannot be confused.
TEST ///
K = QQ;
F0 = K^1;
F1 = K^2;
F2 = K^3;
F = ZZdfactorization hashTable {
    1 => map(F0,F1,{{1,0}}),
    2 => map(F1,F2,0),
    3 => map(F2,F0,0)
    };
assert isWellDefined F
assert((dd^F)^3 == 0)
assert(apply(toList(0..2), i -> rank homology(i,1,F)) === {1,1,3})
assert(apply(toList(0..2), i -> rank homology(i,2,F)) === {0,2,3})
assert(rank homology(-3,1,F) == 1)
assert(rank homology(4,2,F) == 2)
H = HH_1 F;
assert(period H == 3)
assert(dd^H == 0)
assert(apply(toList(0..2), i -> rank H_i) === {1,1,3})
assert(try (homology(0,0,F); false) else true)
assert(try (homology(0,3,F); false) else true)

-- A length-three identity string has zero homology for either amplitude.
G = ZZdfactorization {
    map(K^1,K^1,1), map(K^1,K^1,1), map(K^1,K^1,0)
    };
assert((dd^G)^2 != 0 and (dd^G)^3 == 0)
assert all({1,2}, j -> all(0..2, i -> homology(i,j,G) == 0))
///

-- The quotient must retain the grading of the middle module.
TEST ///
S = QQ[x,y, Degrees => {{1,0},{0,1}}];
F0 = S^1;
F1 = S^{{-1,0}};
F2 = S^{{-1,-1}};
F = ZZdfactorization hashTable {
    1 => map(F0,F1,{{x}}, Degree => {0,0}),
    2 => map(F1,F2,{{y}}, Degree => {0,0}),
    3 => map(F2,F0,0)
    };
assert isWellDefined F
assert((dd^F)^3 == 0)
H1 = homology(0,1,F);
H2 = homology(0,2,F);
H3 = homology(1,2,F);
assert(annihilator H1 == ideal(x*y))
assert(annihilator H2 == ideal x)
assert(annihilator H3 == ideal y)
assert(degrees H1 === {{0,0}})
assert(degrees H3 === {{1,0}})
assert all({H1,H2,H3}, isHomogeneous)
///
