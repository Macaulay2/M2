-- Constructors must infer degrees relative to the cyclic source and target,
-- rather than retaining the degrees of a presentation with different modules.
TEST ///
S = QQ[x,y];
C = ZZdfactorization {x,y^2};
assert isHomogeneous dd^C
assert(degree dd^C_1 === {1} and degree dd^C_2 === {2})
assert(potential C == x*y^2)
D = ZZdfactorization {matrix{{x}},matrix{{y^2}}};
assert isHomogeneous dd^D
assert(degree dd^D_1 === {2} and degree dd^D_2 === {1})
assert isWellDefined D
assert(potential D == x*y^2)
assert isHomogeneous dd^(ZZdfactorization(D, Base => 1))
assert isHomogeneous dd^(ZZdfactorization {0_S,x})
assert not isHomogeneous dd^(ZZdfactorization {x+y^2,x})
m = map(S^2,S^2,{{x,0},{0,y^2}});
assert not isHomogeneous dd^(ZZdfactorization {m,m})
///

-- Preserve nonzero degrees through direct sums and block maps, including
-- zero blocks, without claiming that mixed-degree sums are homogeneous.
TEST ///
S = QQ[x,y];
C = tailMF ideal vars(S/(x^3+y^3));
f = S_0 * id_C;
U = C ++ C;
assert isHomogeneous dd^U
assert all({0,1}, i -> degree dd^U_i === degree dd^C_i)
g = f ++ f;
assert isHomogeneous g
assert all({0,1}, i -> degree g_i === {1})
assert(g == S_0 * id_U)
assert isWellDefined g
assert isFactorizationMorphism g
assert(components g === {f,f})
assert all({0,1}, i -> components g_i === {f_i,f_i})
assert(g_[0]^[0] == f and g_[1]^[1] == f)
h = map(U,U,{{f,0},{0,f}});
assert(h == g)
assert isHomogeneous h
assert all({0,1}, i -> degree h_i === {1})
assert isHomogeneous(f ++ 0*f)
assert not isHomogeneous(f ++ S_0*f)
assert not isHomogeneous map(U,U,{{f,0},{0,S_0*f}})
labelled = directSum {"left" => f, "right" => f};
assert isHomogeneous labelled
assert(labelled_["left"]^["left"] == f)
///

-- Recasting a map between twists must adjust the internal degree relative
-- to the new modules, including negative degrees and multidegrees.
TEST ///
S = QQ[x,y];
C = tailMF ideal vars(S/(x^3+y^3));
f = S_0 * id_C;
Ct = C ** S^{-2};
g = map(C,Ct,f);
assert isHomogeneous g
assert all({0,1}, i -> degree g_i === {-1})
assert(source g === Ct and target g === C)
Dt = C ** S^{-1};
h = map(Dt,C,f);
assert isHomogeneous h
assert all({0,1}, i -> degree h_i === {2})
assert(source h === C and target h === Dt)
assert all({0,1}, i -> entries f_i == entries g_i and entries f_i == entries h_i)
S = QQ[x,y, Degrees => {{1,0},{0,1}}];
C = ZZdfactorization {x,y^2};
assert isHomogeneous dd^C
assert(degree dd^C_1 === {1,0} and degree dd^C_2 === {0,2})
f = x * id_C;
assert isHomogeneous(f ++ f)
assert all({0,1}, i -> degree (f ++ f)_i === {1,0})
Ct = C ** S^{{-2,-1}};
g = map(C,Ct,f);
assert isHomogeneous g
assert all({0,1}, i -> degree g_i === {-1,-1})
M = image matrix{{x,y}};
Z = ZZdfactorization {M,M};
f = x * id_Z;
assert isHomogeneous(f ++ f)
assert isWellDefined(f ++ f)
assert all({0,1}, i -> degree (f ++ f)_i === {1,0})
///
