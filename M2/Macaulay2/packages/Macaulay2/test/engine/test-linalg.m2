R = ZZ/32003[a..e]
I = ideal(b*c-d^2-2, b^2-c*d-1)
G = gb(I, Algorithm=>LinearAlgebra)
gens G

J = ideal flatten entries gens gb I
G = gb(J, Algorithm=>LinearAlgebra)
gens G

m = basis(3,R) * random(R^35,R^3)
G1 = gens time gb(m, Algorithm=>LinearAlgebra);
G2 = gens time gb(ideal flatten entries m);
transpose G1
transpose G2
assert(transpose sort G1 - transpose sort G2 == 0)

m = basis(4,R) * random(R^70,R^3)
time G1 = gens gb(m, Algorithm=>LinearAlgebra);
time G2 = gens gb(ideal flatten entries m);
assert(transpose sort G1 - transpose sort G2 == 0)

m = basis(5,R) * random(R^126,R^3)
time G1 = gens gb(m, Algorithm=>LinearAlgebra);
time G2 = gens gb(ideal flatten entries m);
assert(transpose sort G1 - transpose sort G2 == 0)

m = basis(5,R) * random(R^126,R^4)
time G1 = gens gb(m, Algorithm=>LinearAlgebra);
time G2 = gens gb(ideal flatten entries m);
assert(transpose sort G1 - transpose sort G2 == 0)

m = basis(5,R) * random(R^126,R^10)
time G1 = gens gb(m, Algorithm=>LinearAlgebra);
time G2 = gens gb(ideal flatten entries m);
assert(transpose sort G1 - transpose sort G2 == 0)

load "Markov.m2"
R = markovRing(2,2,2,2)
G = makeGraph {{},{1},{2},{1,2}}
I = markovIdeal(R, localMarkovStmts G);
R = (coefficientRing R)[vars(0..numgens R-1)]
I = substitute(I,vars R)
time G1 = gens gb(I, Algorithm=>Faugere);
time G2 = gens gb(ideal flatten entries gens I);
assert(transpose sort G1 - transpose sort G2 == 0)

end
-- The ones after this still take too much time...

R = ZZ/3[x,y,u,s,t, MonomialSize=>16]
I = ideal(
    x^81,
    y^81,
    u^81,
    u^5-x*y*(x-y)*(s*x-t*y))
G1 = gens time gb(I, Algorithm=>LinearAlgebra);
J = ideal flatten entries gens I; -- 1116 gens
G2 = gens time gb(J); -- 1116 gens...
transpose sort G1 - transpose sort G2 -- different...


G = makeGraph {{},{1},{1},{1},{2,3,4}}
R = markovRing(2,2,2,2,2)
F = marginMap(1,R)
I = F markovIdeal(R, localMarkovStmts G)
time G1 = gens gb(I, Algorithm=>Faugere); -- very bad so far
transpose gens I
I = ideal flatten entries gens I;
time G2 = gens gb I;
transpose sort G1 - transpose sort G2 == 0


G = makeGraph {{},{1},{1},{1},{2,3,4}}
R = markovRing(2,2,2,2,2)
I = markovIdeal(R, localMarkovStmts G)
time G1 = gens gb(I, Algorithm=>Faugere); -- 
I = ideal flatten entries gens I;
time G2 = gens gb I;
transpose sort G1 - transpose sort G2 == 0

transpose gens oo
I2 = markovIdeal(R, localMarkovStmts G);
gbTrace=3
time gens gb I2
I = ideal flatten entries gens I
transpose gens gb I

load "Markov.m2"
R = markovRing(3,3,3,2,2)
G = makeGraph {{},{1},{2},{1,2},{1,2}}
I = markovIdeal(R, localMarkovStmts G);
gb(I, Algorithm=>Faugere)





