R = QQ[x]
M = subquotient( matrix {{x,x^2}} , matrix {{x^3,x^4}} )
p = presentation M
assert( target p ===  cover M )

H = Hom(M,M)
peek H.cache
f = id_M
-- not implemented yet:
-- Hom(M,f)
-- implemented by David:
Hom(f,M)

-- tests for Hom(Matrix,Module) from David:
kk = QQ
S = kk[a,b]
M = S^1/ideal(a)
f = inducedMap(S^1,module ideal(a^3,b^3))
assert( (fh=Hom(f,M) ) === map(cokernel map(S^{{3}},S^{{2}},{{a}}),cokernel map(S^1,S^{{ -1}},{{a}}),{{b^3}}) )

S=kk[a,b,c]
I = ideal"a3,b3,c3"
f = map(S^1,module I,{{a^3,b^3,c^3}})
assert( (Hom(f, S^1)) === map(S^1,S^1,{{1}}) )

