R=QQ[w,x,y,z];
I=ideal(x^2-y*w,x^3-z*w^2)
minimalPrimes I
I = ideal(x^2+y^2)
minimalPrimes I
I = monomialIdeal ideal"wxy,xz,yz"
minimalPrimes I
P = intersect(monomialIdeal(w,x,y),monomialIdeal(x,z),monomialIdeal(y,z))
minI = apply(flatten entries gens P, monomialIdeal @@ support)
dual radical I
P == oo
