R = QQ[x]
M = subquotient( matrix {{x,x^2}} , matrix {{x^3,x^4}} )
p = presentation M
assert( target p ===  cover M )

H = Hom(M,M)
peek H.cache
f = id_M
-- implemented by David:
Hom(M,f)
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
assert( (minimalPresentation Hom(f,M)) === 
   map(S^1,cokernel map(S^{{ -1},{ -1}},S^{{ -2}},{{ -b}, {a}}),{{0,b}}) )
assert( (minimalPresentation Hom(f,f)) === 
     map(S^1,cokernel map(S^{{ -1},{ -1}},S^{{ -2}},{{ -b}, {a}}),{{0,b}}) )

