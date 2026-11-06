-- Tensoring homogeneous scalar factors of a cubic must keep their grading;
-- otherwise the resulting Koszul factorization cannot support graded Hom.
TEST ///
S = ZZ/101[a,b,c];
C = koszulMF({a,b,c}, a^3+b^3+c^3);
assert isWellDefined C
assert isHomogeneous dd^C
assert(potential C == a^3+b^3+c^3)
setRandomSeed 0;
f = randomFactorizationMap(C,C, Cycle => true, InternalDegree => 1);
assert(f != 0)
assert isHomogeneous f
assert all({0,1}, i -> degree f_i === {1})
assert isWellDefined f
assert isCommutative f

-- Different weights require different shifts in the tensor summands.
T = ZZ/101[x,y, Degrees => {2,3}];
D = koszulMF({x,y}, x^3+y^2);
assert isWellDefined D
assert isHomogeneous dd^D
assert(potential D == x^3+y^2)
assert(degree dd^D_1 === {2} and degree dd^D_2 === {4})
assert isHomogeneous dd^(Hom(D,D))
///

-- The tensor grading keeps the first factor's differential profile, so the
-- unit remains strict and the associator has degree zero.
TEST ///
S = ZZ/101[a,b,c];
A = ZZdfactorization {a,a^2};
B = ZZdfactorization {b,b^2};
C = ZZdfactorization {c,c^2};
U = ZZdfactorization(S^1,2);
assert(A**U == A and U**A == A)
assert(id_A**id_B == id_(A**B))
alpha = tensorAssociativity(A,B,C);
sigma = tensorCommutativity(A,B);
tau = tensorCommutativity(B,A);
debugLevel = 1;
assert isWellDefined alpha
assert isHomogeneous alpha
assert isCommutative alpha
assert all({0,1}, i -> degree alpha_i === {0})
assert isWellDefined sigma
assert isHomogeneous sigma
assert isCommutative sigma
assert(tau*sigma == id_(A**B))
debugLevel = 0;
///

-- Multigraded factors can have different differential profiles. Rebracketing
-- and swapping retain the corresponding internal twists.
TEST ///
S = ZZ/101[x,y, Degrees => {{1,0},{0,1}}];
A = ZZdfactorization {x,y};
B = ZZdfactorization {y,x};
AB = A**B;
assert isWellDefined AB
assert isHomogeneous dd^AB
assert(degree dd^AB_1 === {1,0} and degree dd^AB_2 === {0,1})
U = ZZdfactorization(S^1,2);
assert(A**U == A and U**A == A)
alpha = tensorAssociativity(A,B,A);
sigma = tensorCommutativity(A,B);
debugLevel = 1;
assert isHomogeneous alpha
assert isWellDefined alpha
assert isCommutative alpha
assert isHomogeneous sigma
assert isWellDefined sigma
assert isCommutative sigma
debugLevel = 0;
h = (x*id_A)**(y*id_B);
assert(h == (x*y)*id_AB)
assert isHomogeneous h
assert all({0,1}, i -> degree h_i === {1,1})
///

-- Tensor signs use the source summand indices, including for roots of unity
-- other than -1 and for negative homological degrees.
TEST ///
S = ZZ/7[x,y,z];
S.rootOfUnity = 2_S;
C = ZZdfactorization {x,y,z};
T = C**C;
assert isWellDefined T
assert isHomogeneous dd^T
assert(potential T == 2*potential C)
assert(id_C**id_C == id_T)
alpha = tensorAssociativity(C,C,C);
assert isHomogeneous alpha
assert isWellDefined alpha
assert isCommutative alpha
debugLevel = 1;
scan({{0,0},{1,0},{0,1},{-1,1},{1,2},{2,2}}, pair -> (
    setRandomSeed 0;
    f = randomFactorizationMap(C,C, Degree => pair#0,
        InternalDegree => 4, Cycle => true);
    g = randomFactorizationMap(C,C, Degree => pair#1,
        InternalDegree => 4, Cycle => true);
    h = f**g;
    assert(h != 0);
    assert(degree h === sum pair);
    assert isHomogeneous h;
    assert isWellDefined h;
    assert isCommutative h;
    ))
debugLevel = 0;
///
