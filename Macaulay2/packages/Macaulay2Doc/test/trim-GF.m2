-- From a bug reported by Dimitri.Markouchevitch@math.univ-lille1.fr
-- Date: October 19, 2009 3:22:22 PM CDT
p = 13;
KK= GF( (ZZ/p[a])/ideal(a^4-a^3+a^2-a+1));
A2 = matrix {{0, 0, -3, 4*a^3-6*a^2-2*a+3, a^3-5*a^2+2*a-1, 6*a^3-6*a^2-4*a+6}, {0, -3, 0, 6*a^3-4*a^2+3*a, 3*a^3+5*a^2-6*a+5, 3*a^3-4*a^2+a+1}, {-3, 0, 0, a^3+2*a^2-5*a, -5*a^3+a^2+5*a-1, -3*a^3+6*a^2-4*a-5}, {0, 0, -3, -6*a^3+4*a^2+2*a-6, a^3+a^2+4*a, 3*a^3+6*a^2-1}, {0, -3, 0, -3*a^3+6*a^2-2*a+6, 4*a^3+3*a^2+5*a-5, -2*a^3+3*a^2+a+4}, {-3, 0, 0, -6*a^3+a^2-3*a+1, -3*a^3-5*a^2+4*a-6, -a^3-3*a^2-3*a+5}}
kA2 = ker A2     
assert(numgens trim image gens gb gens kA2 === 1 )
-- deferred:
-- assert(numgens trim kA2 === 1)
-- MES: need to change engine code about this, or incorporate more info into the engine.
