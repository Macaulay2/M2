-- test of content functions
debug Core
R = QQ[x,y]
f = 2/5*x + 2/3*y
assert(raw (2/15) == rawAssociateDivisor raw f)

R = ZZ[x,y]
f = -13*37*x + 13^2*38*y
assert(raw (-1) == rawAssociateDivisor raw f)

R = ZZ/101[x,y]
f = -13*37*x + 13^2*38*y
rawAssociateDivisor raw f
assert(raw (24_(coefficientRing R)) == rawAssociateDivisor raw f)

kk = frac(ZZ[a,b])
R = kk[x,y]
f = a*x + 1/b*y
rawAssociateDivisor raw f -- WRONG! this should be 1/b

kk = frac(QQ[a,b])
R = kk[x,y]
f = a*x + 1/b*y
rawAssociateDivisor raw f -- WRONG! this should be 1/b

R = QQ[x,y]
f = 2/5*x + 2/3*y
assert(raw(2/15) == rawContent raw f)
assert(raw(3*x+5*y) == rawRemoveContent raw f)
assert((raw(2/15),raw(3*x+5*y)) == rawSplitContent raw f)

f = matrix{{2/5*x + 2/3*y}}
assert(raw matrix{{2/15}} == rawContent raw f)
assert(raw matrix{{3*x+5*y}} == rawRemoveContent raw f)
assert((raw matrix{{2/15}}, raw matrix{{3*x+5*y}}) == rawSplitContent raw f)

kk = frac(ZZ[a,b])
R = kk[x,y]
f = a*x + 1/b*y
assert(raw(1/b) == rawContent raw f)
assert(raw(a*b*x+y) == rawRemoveContent raw f)
assert((raw(1/b), raw(a*b*x+y)) == rawSplitContent raw f)

-- This is now set to be not implemented:
try (x/(a+1))

f = 1/(a+1)*x  + 1/(b+1)*y + 1/(a^2-b^2)*x*y
new kk from rawContent raw f
new R from rawRemoveContent raw f
rawSplitContent raw f

M = matrix{{1/a},{1/b},{1/(a+b)}}
rawContent raw M
rawRemoveContent raw M
rawSplitContent raw M

kk = frac(QQ[a,b])
R = kk[x,y,z]
try (KK = frac R)
KK = frac(QQ[x,y,z,a,b,MonomialOrder=>{3,2}])
S = KK[s,t,u,v]
I = ideal(a*x-s, b*y-t, s*a^3-t*b^3)
gens gb I

f = 12*a*x + 1/(13*b)*y
rawContent raw f
rawRemoveContent raw f
rawSplitContent raw f


kk = frac(QQ[a]/(a^2-2))
1/a

