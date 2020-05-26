-- testing Hom(Module,Module)
-- free modules:

R = ZZ/101[a,b,c]
M = ker vars R
N = ideal vars R * M
f = map(M, N, gens N//gens M) -- the inclusion of N into M
assert isHomogeneous f
assert isWellDefined f
M' = coker presentation M
N' = coker presentation N
f' = map(M',M,1) * f * map(N,N',1)
assert isHomogeneous f'
assert isWellDefined f'

assert ( hash (Hom(M,N)).cache.cache === hash (Hom(M,N)).cache.cache )
assert ( hash (M**N).cache.cache === hash (M**N).cache.cache )

assert ( prune coker Hom(M',f') == coker vars R )
assert ( prune coker Hom(M,f') == coker vars R )
assert ( prune coker Hom(M',f) == coker vars R )
assert ( prune coker Hom(M,f) == coker vars R )

assert ( coker Hom(f',M') == 0 )
assert ( coker Hom(f',M) == 0 )
assert ( coker Hom(f,M') == 0 )
assert ( coker Hom(f,M) == 0 )

assert ( prune coker Hom(f',N') == coker vars R )
assert ( prune coker Hom(f,N') == coker vars R )
assert ( prune coker Hom(f',N) == coker vars R )
assert ( prune coker Hom(f,N) == coker vars R )

R = QQ[x]
M = x * R^1
H = Hom(M,M)
    assert isHomogeneous H
    assert isHomogeneous H
    h = H_{0}
    assert isHomogeneous h;
    g = homomorphism h;
    assert isHomogeneous g;
    h' = homomorphism' g
    assert ( h === h' )

R = QQ[x,y]
M = R^{ -1,-2 }
N = R^{ -3,-4 }
H = Hom(M,N)
    assert isHomogeneous H
    assert ( H === Hom(M,N) )
    assert isFreeModule H
    assert( entries homomorphism H_{0} == {{1, 0}, {0, 0}} )
    assert( entries homomorphism H_{1} == {{0, 0}, {1, 0}} )
    assert( entries homomorphism H_{2} == {{0, 1}, {0, 0}} )
    assert( entries homomorphism H_{3} == {{0, 0}, {0, 1}} )
    assert( {{1}, {2}} == degrees source homomorphism H_{0} )
    assert( 2 == degree source homomorphism H_{0} )
    for i from 0 to 3 do (
	 h = H_{i};
	 assert isHomogeneous h;
	 g = homomorphism h;
    	 assert isHomogeneous g;
	 -- wait for implementation of "adjoint" in general
    	 -- assert ( h === homomorphism'_H g )
	 )
i = homomorphism H_{0} + homomorphism H_{3}
    assert isHomogeneous i
    assert ( {2} == degree i )
    assert isIsomorphism i
j = homomorphism H_{1} + homomorphism H_{2}
    assert not isHomogeneous j
    assert isIsomorphism j
M = x*M / (y*M)
N = x*N / (y*N)
H = Hom(M,N)
    assert isHomogeneous H
    assert ( H === Hom(M,N) )
    -- debugLevel = 1
    -- assert( entries homomorphism H_{0} == {{1, 0}, {0, 0}} )
    -- assert( entries homomorphism H_{1} == {{0, 0}, {1, 0}} )
    -- assert( entries homomorphism H_{2} == {{0, 1}, {0, 0}} )
    -- assert( entries homomorphism H_{3} == {{0, 0}, {0, 1}} )
    assert( {{2}, {3}} == degrees source homomorphism H_{0} )
    assert( 2 == degree source homomorphism H_{0} )
    for i from 0 to 3 do (
	 h = H_{i};
	 assert (degree h === {0});
	 assert isHomogeneous h;
	 g = homomorphism h;
    	 assert isHomogeneous g;
	 h' = homomorphism' g;
	 assert isHomogeneous h';
	 -- make h' have the same degree as h
	 h' = map(target h', source h' ** R^{ degree h - degree h' }, h', Degree => degree h);
	 assert isHomogeneous h';
    	 assert ( h === h' )
	 )

-----------------------------------------------------------------------------

-- test "compose"

R=QQ[x,y]
M=image vars R ++ R^2
time f = compose(M,M,M);
-- broken by trimming Hom
-- time f' = compose(M,M,M,Strategy=>0);
-- assert ( f === f' )
H = Hom(M,M);
assert isHomogeneous H
for i to numgens H - 1 do for j to numgens H - 1 do (
     g = H_{i};
     g' = H_{j};
     h = homomorphism g;
     h' = homomorphism g';
     assert ( homomorphism (f * (g ** g')) == h' * h ))
H' = trim H
assert isHomogeneous H'
assert ( ambient H === ambient H' )
toH = inducedMap(H,H')
toH' = inducedMap(H',H)
assert ( isIsomorphism toH )
assert ( isIsomorphism toH' )
assert ( toH * toH' == 1 )
assert ( toH' * toH == 1 )
for i to numgens H' - 1 do for j to numgens H' - 1 do (
     g = H'_{i};
     g' = H'_{j};
     h = homomorphism (toH * g);
     h' = homomorphism (toH * g');
     assert ( homomorphism (f * ((toH * g) ** (toH * g'))) == h' * h ))

S = ZZ/101[a,b,c]
A = matrix"a,b,c;b,c,a" 
B = matrix"a,b;b,c"
N = subquotient(A,B)
time com = compose(N,N,N)
assert isHomogeneous com
-- broken by trimming Hom
-- time com' = compose(N,N,N,Strategy => 0)
-- assert ( com === com' )
assert( (minimalPresentation com) === 
    map(cokernel map((S)^1,(S)^{{ -2}},{{b^2-a*c}}),
	cokernel map((S)^1,(S)^{{ -2}},{{b^2-a*c}}),
	{{1}}));


-----------------------------------------------------------------------------

-- test homomorphism' and homomorphism are inverse

R = QQ[x]

f = x ++ x^2
assert ( degree f == {0} )
assert isHomogeneous f
g = homomorphism' f
assert ( degree g == {0} )
assert isHomogeneous g
assert ( source g === R^1 )
assert ( target g === Hom(source f,target f) )
f' = homomorphism g
assert ( f === f' )

f = x ++ x^2
f = map(target f ** R^{2}, source f, f, Degree => {-2})
assert ( degree f == {-2} )
assert isHomogeneous f
g = homomorphism' f
assert (degree g == {-2} )
assert isHomogeneous g
assert ( source g === R^1 )
assert ( target g === Hom(source f,target f) )
f' = homomorphism g
assert ( f === f' )

M = R^{1,2}
N = R^{3,5}
H = Hom(M,N)
assert isHomogeneous H
g = H_{0}
assert ( source g === R^{2} )
assert ( target g === H )
assert isHomogeneous g
f = homomorphism g
assert ( source f === M )
assert ( target f === N )
g' = homomorphism' f
assert isHomogeneous g'
assert ( target g' === H )
assert ( source g' === R^1 )
degree g
degree g'
assert ( g === map(target g, source g, g', Degree => degree g ))

S = QQ[x]
M = S^1/x++S^1
presentation M
g = M^{1}
N = target g
Hom(M,N)
assert isWellDefined g
f = homomorphism' g
assert ( target f === Hom(M,N) )
assert ( source f === S^1 )
g' = homomorphism f
assert ( g == g' )

S = QQ[a,b]
M = S^2/(a,b)++S^2/(b)
g = id_M
f = homomorphism' g
assert isHomogeneous f
assert ( g == homomorphism f )


-----------------------------------------------------------------------------

R = QQ[x]
M = subquotient( matrix {{x,x^2}} , matrix {{x^3,x^4}} )
p = presentation M
assert( target p ===  cover M )

H = Hom(M,M)
peek H.cache
f = id_M
-- implemented by David:
-- disabled temporarily:
-- Hom(M,f)
Hom(f,M)

-- tests for Hom(Matrix,Module) from David:
S = ZZ/101[a,b]
M = S^1/ideal(a)
f = inducedMap(S^1,module ideal(a^3,b^3))
fh = Hom(f,M)
assert( (minimalPresentation Hom(f,M) ) === map(cokernel map(S^{{3}},S^{{2}},{{a}}),cokernel map(S^1,S^{{-1}},{{a}}),{{b^3}}) )
assert( target fh === Hom(source f,M) )
assert( source fh === Hom(target f,M) )

S=ZZ/101[a,b,c]
I = ideal"a3,b3,c3"
f = map(S^1,module I,{{a^3,b^3,c^3}})
fh = Hom(f,S^1)
assert( target fh === Hom(source f,S^1) )
assert( source fh === Hom(target f,S^1) )
assert( fh === map(image map(S^{{3},{3},{3}},S^1,{{a^3}, {b^3}, {c^3}}),S^1,{{1}}) )


M = module ((ideal(a,b))^3)
M'= module ((ideal(a,b))^2)
f=inducedMap(M',M)
fh = Hom(M,f)
assert (target fh == Hom(M, target f))
assert (source fh == Hom(M, source f))
-- assert( (minimalPresentation Hom(f,M)) === 
--    map(S^1,cokernel map(S^{{ -1},{ -1}},S^{{ -2}},{{ -b}, {a}}),{{0,b}}) )
-- assert( (minimalPresentation Hom(f,f)) === 
--      map(S^1,cokernel map(S^{{ -1},{ -1}},S^{{ -2}},{{ -b}, {a}}),{{0,b}}) )

-- bug found by David Treumann:

p = 1
q = 1
S = QQ[x,y, Degrees => {{1,0},{0,1}}]
E = S^{{ -1,0},{-2,0},{0,-1},{-1,-1},{-1,-1},{-2,-1},{0,-2},{-1,-2}}
Xmatrix = transpose matrix{
     {0_S,1,0,0,0,0,0,0},
     {0,0,0,0,0,0,0,0},
     {0,0,0,1,0,0,0,0},
     {0,0,0,0,0,p,0,0},
     {0,0,0,0,0,1,0,0},
     {0,0,0,0,0,0,0,0},
     {0,0,0,0,0,0,0,1},
     {0,0,0,0,0,0,0,0}
     }
Ymatrix =transpose matrix{
     {0_S,0,0,0,1,0,0,0},
     {0,0,0,0,0,1,0,0},
     {0,0,0,0,0,0,1,0},
     {0,0,0,0,0,0,0,1},
     {0,0,0,0,0,0,0,q},
     {0,0,0,0,0,0,0,0},
     {0,0,0,0,0,0,0,0},
     {0,0,0,0,0,0,0,0}
     }
phi = map(E,E**S^{{ -1,0 }}++E**S^{{0,-1}},(x*id_E-Xmatrix) | (y*id_E-Ymatrix));
isHomogeneous phi
M = coker phi
Ext^1(M,M)   -- mismatch here, because trimming of result is ignored

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test hom.out"
-- End:

