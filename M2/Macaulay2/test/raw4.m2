-- MES
-- Test of the rawGB commands

load "raw-util.m2"
R1 = rawPolynomialRing(rawQQ(), singlemonoid{x,y,z})
x = rawRingVar(R1,0,1)
y = rawRingVar(R1,1,1)
z = rawRingVar(R1,2,1)

algorithm = 0
algorithm = 1

-- Test 1.  A very simple GB.
G = mat {{x,y,z}}
Gcomp = rawGB(G,false,0,false,0,algorithm,0)

m = rawGBGetMatrix(Gcomp,1,false) -- Groebner basis
assert(m == mat{{z,y,x}})

Gcomp = rawGB(G,true,3,false,0,0,0)
m = rawGBGetMatrix(Gcomp,1,false) 
assert(m == mat{{z,y,x}})

msyz = rawGBGetMatrix(Gcomp,2,false)
zeroelem = rawFromNumber(R1,0)
assert(msyz == mat{{zeroelem,z,y},{z,zeroelem,-x},{-y,-x,zeroelem}})

-- Test 1A.
G = mat {{x,y,z^2, x*z+z^2}}
Gcomp = rawGB(G,false,0,false,0,0,0)

m = rawGBGetMatrix(Gcomp,1,false) -- Groebner basis
assert(m == mat{{y,x,z^2}})
m = rawGBGetMatrix(Gcomp,1,true) -- mingens
assert(m == mat{{y,x,z^2}})

-- Test 2. 
G = mat {{x*y-y^2, x^2}}
Gcomp = rawGB(G,false,0,false,0,algorithm,0)
m = rawGBGetMatrix(Gcomp,1,false) -- Groebner basis
assert(m == mat{{x*y-y^2, x^2, y^3}})

Gcomp = rawGB(G,true,-1,false,0,0,0)
m = rawGBGetMatrix(Gcomp,2,false) -- syz
m = rawGBGetMatrix(Gcomp,1,false) -- gb

-- Test 3. 

G = mat{{(3*x+y+z)^2, (7*x+2*y+3*z)^2}}
Gcomp = rawGB(G,false,0,false,0,0,0)
m = rawGBGetMatrix(Gcomp,1,false) -- gb
answerm = mat{{21*x^2-2*y^2+42*x*z+8*y*z+13*z^2,
	       42*x*y+13*y^2-84*x*z-10*y*z-32*z^2,
	       y^3-6*y^2*z+12*y*z^2-8*z^3}}
assert(m == answerm)

-- Test 4. -- module GB
R2 = rawPolynomialRing(rawQQ(), singlemonoid{a,b,c,d,e,f})
a = rawRingVar(R2,0,1)
b = rawRingVar(R2,1,1)
c = rawRingVar(R2,2,1)
d = rawRingVar(R2,3,1)
e = rawRingVar(R2,4,1)
f = rawRingVar(R2,5,1)
m = mat {{a,b,c},{d,e,f}}
Gcomp = rawGB(m,false,0,false,0,0,0)
mgb = rawGBGetMatrix(Gcomp,1,false) -- gb

Gcomp = rawGB(m,true,-1,false,0,0,0)
mgb = rawGBGetMatrix(Gcomp,1,false) -- gb
msyz = rawGBGetMatrix(Gcomp,2,false) -- gb
mchange = rawGBGetChange(Gcomp,1)
m
assert(mgb == m * mchange)

-- Test 5. -- Semi-random cubics
M = mat {{(5*a+b+3*c)^10, (3*a+17*b+4*d)^10, (9*b+13*c+12*d)^10}}
Gcomp = rawGB(M,false,0,false,0,0,0)
time mgb = rawGBGetMatrix(Gcomp,1,false); -- gb
rawGBGetLeadTerms(Gcomp,6,1)

-- Test 6. -- same in char 101
rawMonoid()
R2 = rawPolynomialRing(rawZZp(101,rawMonoid()), singlemonoid{a,b,c,d,e,f})
a = rawRingVar(R2,0,1)
b = rawRingVar(R2,1,1)
c = rawRingVar(R2,2,1)
d = rawRingVar(R2,3,1)
e = rawRingVar(R2,4,1)
f = rawRingVar(R2,5,1)
M = mat {{(5*a+b+3*c)^10, (3*a+17*b+4*d)^10, (9*b+13*c+12*d)^10}} -- bus error!!
Gcomp = rawGB(M,false,0,false,0,0,0)
time mgb = rawGBGetMatrix(Gcomp,1,false); -- gb


rawGBSetStop(Gcomp, ...) -- MES: make sure the default is set correctly
-- Actual computation is done in, eg:

rawGBGetMatrix(Gcomp,1,true) -- mingens
rawGBGetMatrix(Gcomp,2,false) -- syz matrix

-- Test: a bug.
load "raw-util.m2"
R2 = rawPolynomialRing(rawQQ(), singlemonoid{a,b,c,d,e,f})
a = rawRingVar(R2,0,1)
b = rawRingVar(R2,1,1)
c = rawRingVar(R2,2,1)
d = rawRingVar(R2,3,1)
e = rawRingVar(R2,4,1)
f = rawRingVar(R2,5,1)
M = mat {{(5*a+b+3*c)^10, (3*a+17*b+4*d)^10, (9*b+13*c+12*d)^10}}
  -- BUG BUG: for some reason, toString is being called several times here
--Gcomp = rawGB(M,false,0,false,0,0,0)
R2 = rawPolynomialRing(rawZZp(101,rawMonoid()), singlemonoid{a,b,c,d,e,f})
a = rawRingVar(R2,0,1)
b = rawRingVar(R2,1,1)
c = rawRingVar(R2,2,1)
d = rawRingVar(R2,3,1)
e = rawRingVar(R2,4,1)
f = rawRingVar(R2,5,1)
M = mat {{(5*a+b+3*c)^10, (3*a+17*b+4*d)^10, (9*b+13*c+12*d)^10}}; -- bus error!!
M
  -- BUG BUG: for some reason, toString is being called several times here, AND
  -- also == on RawMatrix is begin called here too.  Why??


--- Tests for gbA: inhomogeneous and local
load "raw-util.m2"
R1 = rawPolynomialRing(rawQQ(), singlemonoid{x,y,z})
x = rawRingVar(R1,0,1)
y = rawRingVar(R1,1,1)
z = rawRingVar(R1,2,1)
algorithm = 1

G = mat {{x*y-1, x^2-x, x^3-z-1}}
Gcomp = rawGB(G,false,0,false,0,algorithm,0)
m = rawGBGetMatrix(Gcomp,1,false) -- Groebner basis

R2 = rawPolynomialRing(rawQQ(), lex{x,y,z})
x = rawRingVar(R2,0,1)
y = rawRingVar(R2,1,1)
z = rawRingVar(R2,2,1)
algorithm = 1
G = mat {{x*y-1, x^2-x, x^3-z-1}}
Gcomp = rawGB(G,false,0,false,0,algorithm,0)
m = rawGBGetMatrix(Gcomp,1,false) -- Groebner basis


G = mat {{x^2*y-y^3-1, x*y^2-x-1}}
Gcomp = rawGB(G,false,0,false,0,algorithm,0)
m = rawGBGetMatrix(Gcomp,1,false) -- Groebner basis
-- check: this is correct.  ACTUALLY: it is NOT CORRECT: the first element is not 
-- monic!!  Also, I haven't checked the actual polynomials yet.
assert(m == mat{{y^7-2*y^5+y^4+y^3-2*y^2-y+1, x-y^6+y^4-y^3+y+1}})

G = mat {{x^2*y-17*y^3-23, 3*x*y^2-x-6}}
Gcomp = rawGB(G,false,0,false,0,algorithm,0)
m = rawGBGetMatrix(Gcomp,1,false) -- Groebner basis

-- fixed: BUG -- actually 2 separate bugs here: depending in the ring
--        used (now works with both R1,R2).
f1 = mat {{x+y+z, x*y+y*z+z*x, x*y*z-1, (x+y+z)^5+x*(x*y*z-1) + 13}}
G1 = rawGB(f1,true,-1,false,0,algorithm,0)
gb1 = rawGBGetMatrix(G1,1,false) -- Groebner basis
ch1 = rawGBGetChange(G1,1)
f2 = rawGBGetMatrix(G1,2,false)
assert(f1 * ch1 - gb1 == 0)
assert(f1 * f2 == 0)

G2 = rawGB(f2,true,-1,false,0,algorithm,0)
gb2 = rawGBGetMatrix(G2,1,false) -- Groebner basis
ch2 = rawGBGetChange(G2,1)
f3 = rawGBGetMatrix(G2,2,false)
assert(f2 * ch2 - gb2 == 0) 
assert(f2 * f3 == 0) 
--------------------------------


G = mat {{x+y+z, x*y+y*z+z*x, x*y*z-1, (x+y+z)^5+x*(x*y*z-1) + 13}}
Gcomp = rawGB(G,true,-1,false,0,algorithm,0)
m = rawGBGetMatrix(Gcomp,1,false) -- Groebner basis
ch = rawGBGetChange(Gcomp,1)
G * ch
syzm = rawGBGetMatrix(Gcomp,2,false) -- syz matrix
assert(G * syzm == 0)
Gcomp = rawGB(syzm,true,-1,false,0,algorithm,0)
ch = rawGBGetChange(Gcomp,1)
g2 = rawGBGetMatrix(Gcomp,1,false)
syzm * ch - g2  -- 8..10 elements are NOT zero: BUG
syz2m = rawGBGetMatrix(Gcomp,2,false) -- syz matrix

syzm * syz2m -- not zero!! BUG

G = mat {{3*x-y^20, 4*y-z^20, x*y-x-1}}
Gcomp = rawGB(G,false,0,false,0,algorithm,0)
m = rawGBGetMatrix(Gcomp,1,false) -- Groebner basis

-- Koszul syzygies
load "raw-util.m2"
R1 = rawPolynomialRing(rawQQ(), singlemonoid{x,y,z,w,t})
x = rawRingVar(R1,0,1)
y = rawRingVar(R1,1,1)
z = rawRingVar(R1,2,1)
w = R1_3
t = R1_4
algorithm = 1
G = mat {{x-1,y-t^3-2,2*z-t^7-3,5*w-t-7}}
Gcomp = rawGB(G,true,-1,false,0,algorithm,0)
m = rawGBGetMatrix(Gcomp,1,false) -- Groebner basis
m1 = rawGBGetMatrix(Gcomp,2,false)
assert(G * m1 == 0)
Gcomp = rawGB(m1,true,-1,false,0,algorithm,0)
m2 = rawGBGetMatrix(Gcomp,2,false) -- syz
assert(m1 * m2 == 0)
Gcomp = rawGB(m2,true,-1,false,0,algorithm,0)
m3 = rawGBGetMatrix(Gcomp,2,false) -- syz
assert(m2 * m3 == 0)

-- M2 code:
///
R = QQ[x,y,z,MonomialOrder=>Lex]
G = matrix{{x^2*y-17*y^3-23, 3*x*y^2-x-6}}
gens gb G

G = matrix {{x+y+z, x*y+y*z+z*x, x*y*z-1, (x+y+z)^5+x*(x*y*z-1) + 13}}
gens gb G
syz G
syz oo
ooo * oo
///

load "raw-util.m2"
R1 = rawPolynomialRing(rawQQ(), elim({t},{a,b,c,d}))
t = rawRingVar(R1,0,1)
a = rawRingVar(R1,1,1)
b = rawRingVar(R1,2,1)
c = rawRingVar(R1,3,1)
d = rawRingVar(R1,4,1)
algorithm = 1
G = mat{{b^4-13*a*c, 12*b*c^2-7*a*d^3, t*a-1}}
Gcomp = rawGB(G,false,0,false,0,algorithm,0)
m = rawGBGetMatrix(Gcomp,1,false) -- Groebner basis

R2 = rawPolynomialRing(rawQQ(), lex{u,v,x,y,z})
u = rawRingVar(R2,0,1)
v = rawRingVar(R2,1,1)
x = rawRingVar(R2,2,1)
y = rawRingVar(R2,3,1)
z = rawRingVar(R2,4,1)
G = mat{{x - 3*u-3*u*v^2+u^3, y-3*v-3*u^2*v+v^3, z-3*u^2+3*v^2}}
Gcomp = rawGB(G,false,0,false,0,algorithm,0)
time m = rawGBGetMatrix(Gcomp,1,false); -- Groebner basis
rawGBGetLeadTerms(Gcomp,6,1)

time rawGBGetMatrix(rawGB(oo,false,0,false,0,algorithm,0), 1,true)
-- Is this correct?

-- Try the other routines:
-- rawGB, rawGBSetHilbertFunction, rawGBForce
-- rawGBSetStop
-- rawGBGetMatrix, rawGBGetChange, rawGBGetLeadTerms, rawGBGetFree
-- rawGBStatus, rawGBStatusLevel, rawGBBetti
-- rawGBMatrixRemainder, rawGBMatrixLift, rawGBContains
-- rawGBverbose

load "raw-util.m2"
R1 = rawPolynomialRing(rawQQ(), singlemonoid{a,b,c,d})
a = rawRingVar(R1,0,1)
b = rawRingVar(R1,1,1)
c = rawRingVar(R1,2,1)
d = rawRingVar(R1,3,1)
algorithm = 1
G = mat{{b^4-13*a*c, 12*b*c^2-7*a*b*d^3-1}}
Gcomp = rawGB(G,false,0,false,0,algorithm,0)
time m = rawGBGetMatrix(Gcomp,1,false); -- Groebner basis
m
f = (7*a^2+a+1)*(a^20+a^17+4*a^13+1)^7
g = (7*a^2+a+1)*(6*a^20-123*a^17+4*a^13+1)^6
G = mat{{f,g}}
Gcomp = rawGB(G,false,0,false,0,algorithm,0)
time m = rawGBGetMatrix(Gcomp,1,false); -- Groebner basis
m
