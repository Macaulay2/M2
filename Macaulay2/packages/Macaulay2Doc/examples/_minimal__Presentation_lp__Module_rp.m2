R = ZZ/32003[a..d];
M = coker matrix {{a,1,b},{c,3,b+d}}
N = minimalPresentation M
peek N.cache
g = N.cache.pruningMap
g^-1
I = ideal(a^2,b^3,c^4,d^7)
X = Proj R
J = (module I)~
minimalPresentation J
