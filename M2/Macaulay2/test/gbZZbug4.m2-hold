-- May 23, 2006:
R = ZZ[s,t,x,y,z, MonomialOrder=>{2,3}];
I = ideal(x-s^3-s*t-1, y-t^3-3*t^2-t, z-s*t^3)
leadTerm gens gb I  --crashes on my macintel.

gbTrace=3
gens gb(I, DegreeLimit=>4)
gens gb(I, DegreeLimit=>5)
gens gb(I, DegreeLimit=>6)
gens gb(I, DegreeLimit=>7)
gens gb(I, DegreeLimit=>8)
transpose oo
gens gb(I, DegreeLimit=>9)
gens gb(I, DegreeLimit=>10) -- crash occurs in here

R = ZZ[s,t,x,y,z,h, MonomialOrder=>{2,3,1}];
I = ideal(x-s^3-s*t-1, y-t^3-3*t^2-t, z-s*t^3)
Ih = homogenize(I,h)
time leadTerm gens gb Ih
inI = substitute(oo,h=>1);
inI = matrix entries inI
inI = sort inI
gens gb inI
transpose gens gb inI
J = ideal inI
sort gens gb J
