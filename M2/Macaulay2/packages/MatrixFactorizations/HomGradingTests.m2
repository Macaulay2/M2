-- The wraparound twist in odd Hom degree is the degree of the potential.
-- InternalDegree is measured in the normalized periodic Hom module, so the
-- two raw component degrees need not agree for an odd map.
TEST ///
S = ZZ/101[a,b];
R = S/(a^3+b^3);
C = tailMF ideal vars R;
D = tailMF((ideal vars R)^2);
E = Hom(C,D);
assert isHomogeneous dd^E
assert(degree dd^E_1 === {0} and degree dd^E_2 === {3})
scan({-1,0,1,2}, d -> scan({false,true}, boundary -> (
    setRandomSeed 0;
    f = randomFactorizationMap(D,C, Degree => d, InternalDegree => 3,
        Cycle => not boundary, Boundary => boundary);
    assert(f != 0);
    assert(degree f === d);
    assert isWellDefined f;
    assert isHomogeneous f;
    assert isCommutative f;
    assert(degree f_0 === {3});
    assert(degree f_1 === if odd d then {0} else {3});
    assert(homomorphism homomorphism' f == f);
    )))
///

-- The same construction works with degree vectors and with zero differential
-- components (whose internal degree is unconstrained).
TEST ///
S = ZZ/101[x,y, Degrees => {{1,0},{0,1}}];
F0 = S^1;
F1 = S^{{-1,0}};
C = ZZdfactorization hashTable {
    1 => map(F0,F1,{{x}}, Degree => {0,0}),
    2 => map(F1,F0,{{y}}, Degree => {1,1})
    };
E = Hom(C,C);
assert isHomogeneous dd^E
scan({false,true}, boundary -> (
    setRandomSeed 0;
    f = randomFactorizationMap(C,C, Degree => 1, InternalDegree => {1,1},
        Cycle => not boundary, Boundary => boundary);
    assert(f != 0);
    assert isWellDefined f;
    assert isHomogeneous f;
    assert isCommutative f;
    assert(degree f_0 === {1,1} and degree f_1 === {0,0});
    assert(homomorphism homomorphism' f == f);
    ))
Z = ZZdfactorization(S^1,2);
assert isHomogeneous dd^(Hom(C,Z))
assert isHomogeneous dd^(Hom(Z,C))
///

-- Different differential profiles normalize relative to module zero. Their
-- Hom module therefore needs relative twists even in homological degree zero.
TEST ///
S = ZZ/101[x,y];
F0 = S^1;
F1 = S^{-1};
G1 = S^{-2};
C = ZZdfactorization hashTable {
    1 => map(F0,F1,{{x}}, Degree => 0),
    2 => map(F1,F0,{{x^2}}, Degree => 3)
    };
D = ZZdfactorization hashTable {
    1 => map(F0,G1,{{x}}, Degree => -1),
    2 => map(G1,F0,{{x^2}}, Degree => 4)
    };
assert isHomogeneous dd^(Hom(C,D))
scan({0,1}, d -> (
    setRandomSeed 0;
    f = randomFactorizationMap(D,C, Degree => d, InternalDegree => 2,
        Cycle => true);
    assert(f != 0);
    assert isHomogeneous f;
    assert isWellDefined f;
    assert isCommutative f;
    assert(apply({0,1}, i -> degree f_i) ===
        if d == 0 then {{2},{3}} else {{3},{-1}});
    assert(homomorphism homomorphism' f == f);
    ))
h = Hom(x*id_C, x^2*id_D);
assert isHomogeneous h
assert all({0,1}, i -> degree h_i === {3})
assert(h == x^3*id_(Hom(C,D)))
///

-- Hom preserves identities and the graded pre/postcomposition signs. Force
-- commutativity checks to recompute, rather than accepting a cached answer.
TEST ///
S = ZZ/101[x,y];
C = ZZdfactorization {x,y};
E = Hom(C,C);
assert(Hom(id_C,id_C) == id_E)
setRandomSeed 0;
f = randomFactorizationMap(C,C, Degree => 1, InternalDegree => 2, Cycle => true);
g = randomFactorizationMap(C,C, Degree => 1, InternalDegree => 2, Cycle => true);
assert(f != 0 and g != 0)
h = Hom(f,id_C);
debugLevel = 1;
assert isCommutative h
assert isHomogeneous h
assert isWellDefined h
assert(homomorphism(h * homomorphism' g) == g*f)
debugLevel = 0;

T = ZZ/7[u,v,w];
T.rootOfUnity = 2_T;
D = ZZdfactorization {u,v,w};
H = Hom(D,D);
assert(Hom(id_D,id_D) == id_H)
assert isHomogeneous dd^H
debugLevel = 1;
scan({{0,0},{1,0},{0,1},{-1,1},{1,2},{2,2}}, pair -> (
    setRandomSeed 0;
    f = randomFactorizationMap(D,D, Degree => pair#0,
        InternalDegree => 4, Cycle => true);
    g = randomFactorizationMap(D,D, Degree => pair#1,
        InternalDegree => 4, Cycle => true);
    assert(f != 0 and g != 0);
    h = Hom(f,g);
    assert(degree h === sum pair);
    assert isHomogeneous h;
    assert isWellDefined h;
    assert isCommutative h;
    ))
debugLevel = 0;
///

-- Ungraded Hom remains available when normalization is impossible, but an
-- explicit InternalDegree must not silently promise a compatible grading.
TEST ///
S = ZZ/101[x];
C = ZZdfactorization {x,x};
D = ZZdfactorization {x,x^2};
assert isHomogeneous dd^C
assert isHomogeneous dd^D
assert isWellDefined Hom(C,D)
assert not isHomogeneous dd^(Hom(C,D))
assert try (randomFactorizationMap(D,C, InternalDegree => 1); false) else true

B = ZZdfactorization {x+x^2,x};
assert not isHomogeneous dd^B
assert isWellDefined Hom(B,B)
assert try (randomFactorizationMap(B,B, InternalDegree => 1); false) else true
///
