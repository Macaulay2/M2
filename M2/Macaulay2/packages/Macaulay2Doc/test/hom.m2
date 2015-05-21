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


R = QQ[x,y]
M = R^{ -1,-2 }
N = R^{ -3,-4 }
H = Hom(M,N)
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
    assert ( H === Hom(M,N) )
    debugLevel = 1
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
-- disabled temporarily:
-- fh = Hom(M,f)
-- assert (target fh == Hom(M, target f))
-- assert (source fh == Hom(M, source f))
-- assert( (minimalPresentation Hom(f,M)) === 
--    map(S^1,cokernel map(S^{{ -1},{ -1}},S^{{ -2}},{{ -b}, {a}}),{{0,b}}) )
-- assert( (minimalPresentation Hom(f,f)) === 
--      map(S^1,cokernel map(S^{{ -1},{ -1}},S^{{ -2}},{{ -b}, {a}}),{{0,b}}) )

-- bug found by David Treumann:

p = 1
q = 1
S = QQ[x,y, Degrees => {{1,0},{0,1}}]
E = S^{{-1,0},{-2,0},{0,-1},{-1,-1},{-1,-1},{-2,-1},{0,-2},{-1,-2}}
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
phi = map(E,E**S^{{-1,0}}++E**S^{{0,-1}},(x*id_E-Xmatrix) | (y*id_E-Ymatrix));
isHomogeneous phi
M = coker phi
Ext^1(M,M)   -- mismatch here, because trimming of result is ignored

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test 4-b.out"
-- End:

