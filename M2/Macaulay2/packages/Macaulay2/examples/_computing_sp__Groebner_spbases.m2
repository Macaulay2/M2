R = ZZ/1277[x,y];
I = ideal(x^3 - 2*x*y, x^2*y - 2*y^2 + x);
g = gb I
gens g
R = ZZ/1277[x,y,z,w];
I = ideal(x*y-z^2,y^2-w^2);
g2 = gb(I,DegreeLimit => 2)
gens g2
g3 = gb(I,DegreeLimit => 3);
gens g3
g2
g2 === g3
I = ideal(x*y-z^2,y^2-w^2)
gb(I,PairLimit => 2)
gb(I,PairLimit => 3)
I = ideal(x*y-z^2,y^2-w^2)
gb(I,BasisElementLimit => 2)
gb(I,BasisElementLimit => 3)
R = ZZ/1277[t,F,G,MonomialOrder => Eliminate 1];
I = ideal(F - (t^3 + t^2 + 1), G - (t^4 - t))
transpose gens gb (I, SubringLimit => 1)
gbTrace = 3
I = ideal(x*y-z^2,y^2-w^2)
gb I
gbTrace = 0
R = ZZ/1277[x..z];
I = ideal(x*y+y*z, y^2, x^2);
g = gb(I, StopBeforeComputation => true)
gens g
R = ZZ/1277[a..e];
T = (degreesRing R)_0
f = random(R^1,R^{-3,-3,-5,-6});
time betti gb f
remove(f.cache,{false,0})
(cokernel f).cache.poincare = (1-T^3)*(1-T^3)*(1-T^5)*(1-T^6)
time betti gb f
