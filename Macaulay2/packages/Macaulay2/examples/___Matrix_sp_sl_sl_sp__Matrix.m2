A = ZZ/101[x,y,z]
F = x^4 - y*z*(1-x)^2 - z - y^3
I = ideal(F,diff(x,F),diff(y,F),diff(z,F))
1 % I
g = gens I
f = matrix{{1_A}}
h = f // g
g * (f//g)
1 // (gens I)
M = matrix{{1,x,y},{x,0,y},{1,2,3}}
M = substitute(M, frac A)
det M
Minv = id_(target M) // M
M * Minv
