-- -*- coding: utf-8 -*-
--status: this old test depends on internal things and probably should be deleted


-- MES
-- Test of the rawGB commands

-----------------------------
-- GB's over finite fields --
-- simplest examples --------
-----------------------------
--- simplest example
needs "raw-util.m2"
algorithm = 0
R1 = polyring(rawZZp(101), (symbol x, symbol y, symbol z))

G = mat {{x,y,z}}
Gcomp = rawGB(G,false,0,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp -- Gröbner basis
assert(m === mat{{z,y,x}})
assert(rawGBMatrixRemainder(Gcomp,mat{{x}}) == 0)

  -- Let's test syzygies:
Gcomp = rawGB(G,true,3,{},false,0,0,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp -- Gröbner basis
assert(m === mat{{z,y,x}})

msyz = rawGBSyzygies Gcomp
zeroelem = rawFromNumber(R1,0)
assert(msyz == mat{{zeroelem,z,y},{z,zeroelem,-x},{-y,-x,zeroelem}})

-----------------------------
--- Test 1A.
needs "raw-util.m2"
R1 = polyring(rawZZp(101), (symbol x, symbol y, symbol z))
G = mat {{x,y,z^2, x*z+z^2}}
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp -- Gröbner basis
assert(m == mat{{y,x,z^2}})
m1 = rawGBMinimalGenerators Gcomp -- mingens
assert(m1 == mat{{y,x,z^2}})
-----------------------------
--- Test 2. 
needs "raw-util.m2"
algorithm = 0
R1 = polyring(rawZZp(101), (symbol x, symbol y, symbol z))
G = mat {{x*y-y^2, x^2}}
Gcomp = rawGB(G,false,0,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp -- Gröbner basis
assert(m == mat{{x*y-y^2, x^2, y^3}})

Gcomp = rawGB(G,true,-1,{},false,0,0,0,10)
rawStartComputation Gcomp
m = rawGBSyzygies Gcomp
m = rawGBGetMatrix Gcomp
assert isComputationDone rawStatus1 Gcomp
rawStatus2 Gcomp -- last degree something was computed in

assert(rawGBMatrixRemainder(Gcomp,mat{{x*y}}) === mat{{y^2}})
-----------------------------
--- Test 3. 
-- a simple GB, and testing min gens
needs "raw-util.m2"
algorithm = 0
R1 = polyring(rawZZp(101), (symbol x, symbol y, symbol z))
G = mat {{x*y-y^2, x^2, y^4}}
Gcomp = rawGB(G,false,0,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp -- Gröbner basis
assert(m == mat{{x*y-y^2, x^2, y^3}})
assert(rawGBMinimalGenerators(Gcomp) == mat{{x*y-y^2, x^2}})
-----------------------------
--- Test: 2: simple example of powers of linear forms.
-- 
needs "raw-util.m2"
algorithm = 0
R1 = polyring(rawZZp(101), (symbol x, symbol y, symbol z))

G = mat{{(3*x+y+z)^2, (7*x+2*y+3*z)^2}}
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp
answerm = mat{{(21*x^2-2*y^2+42*x*z+8*y*z+13*z^2)//(21_R1),
	       (42*x*y+13*y^2-84*x*z-10*y*z-32*z^2)//(42_R1),
	       y^3-6*y^2*z+12*y*z^2-8*z^3}}
--assert(m == answerm)     -- FIX THE SORTED ORDER OF A GROEBNER BASIS!!
<< "uncomment assertion" << endl;
-----------------------------
needs "raw-util.m2"
algorithm = 0
R1 = polyring(rawZZp(101), (symbol x, symbol y, symbol z))
G = mat {{x,y^2,z^3}}
Gcomp = rawGB(G,false,0,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp -- Gröbner basis
assert(m === G)
assert(rawGBMatrixRemainder(Gcomp,mat{{x-1}}) === mat{{-1_R1}})
-----------------------------
needs "raw-util.m2"
algorithm = 0
R1 = polyring(rawZZp(101), (symbol x, symbol y, symbol z))
G = mat {{x,y,z^2, x*z+z^2}}
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp -- Gröbner basis
assert(m == mat{{y,x,z^2}})
m1 = rawGBMinimalGenerators Gcomp -- mingens
assert(m1 == mat{{y,x,z^2}})
-----------------------------
-- Test 4. -- module GB
needs "raw-util.m2"
algorithm = 0
R2 = polyring(rawZZp(101), symbol a .. symbol f)
m = mat {{a,b,c},{d,e,f}}
Gcomp = rawGB(m,false,0,{},false,0,0,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix Gcomp

Gcomp = rawGB(m,true,-1,{},false,0,0,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix Gcomp
msyz = rawGBSyzygies Gcomp
mchange = rawGBChangeOfBasis Gcomp
assert(mgb == m * mchange)
-----------------------------
-- Test 5. -- Semi-random cubics
needs "raw-util.m2"
R2 = polyring(rawZZp(101), symbol a .. symbol f)
M = mat {{(5*a+b+3*c)^10, (3*a+17*b+4*d)^10, (9*b+13*c+12*d)^10}}
Gcomp = rawGB(M,false,0,{0},false,0,0,0,10)
rawStartComputation Gcomp  -- crashes due to bad access in spair_sorter -- NO LONGER CRASHES...
mgb = rawGBGetMatrix Gcomp; -- takes TOO LONG (but does finish)
--rawDual mgb
rawGBGetLeadTerms(Gcomp,6)
mmin = rawGBMinimalGenerators Gcomp
rawDual mmin
msyz = rawGBSyzygies Gcomp
-----------------------------
-- GB's over finite fields --
-- inhomogeneous ------------
-----------------------------
needs "raw-util.m2"
R1 = polyring(rawZZp(101), (symbol x,symbol y,symbol z))
algorithm = 0

G = mat {{x*y-1, x^2-x, x^3-z-1}}
Gcomp = rawGB(G,false,0,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix Gcomp
assert(mgb === mat{{z,y-1,x-1}})
-----------------------------
needs "raw-util.m2"
R2 = rawPolynomialRing(rawZZp(101), lex(x,y,z))
x = rawRingVar(R2,0)
y = rawRingVar(R2,1)
z = rawRingVar(R2,2)
algorithm = 1
G = mat {{x*y-1, x^2-x, x^3-z-1}}
Gcomp = rawGB(G,false,0,{0},false,0,algorithm,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix Gcomp
assert(mgb === mat{{z,y-1,x-1}})
-----------------------------
needs "raw-util.m2"
algorithm = 1
R2 = rawPolynomialRing(rawZZp(101), lex(x,y,z))
x = rawRingVar(R2,0)
y = rawRingVar(R2,1)
z = rawRingVar(R2,2)
G = mat {{x^2*y-y^3-1, x*y^2-x-1}}
Gcomp = rawGB(G,false,0,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix Gcomp
assert(mgb === mat{{y^7-2*y^5+y^4+y^3-2*y^2-y+1, x-y^6+y^4-y^3+y+1}})
<< "warning: I haven't checked the actual polynomials yet. !!" << endl;
-----------------------------
needs "raw-util.m2"
algorithm = 1
R2 = rawPolynomialRing(rawZZp(101), lex(x,y,z))
x = rawRingVar(R2,0)
y = rawRingVar(R2,1)
z = rawRingVar(R2,2)
G = mat {{x^2*y-17*y^3-23, 3*x*y^2-x-6}}
Gcomp = rawGB(G,false,0,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix Gcomp

f1 = mat {{x+y+z, x*y+y*z+z*x, x*y*z-1, (x+y+z)^5+x*(x*y*z-1) + 13}}
G1 = rawGB(f1,true,-1,{},false,0,algorithm,0,10)
rawStartComputation G1
gb1 = rawGBGetMatrix G1
ch1 = rawGBChangeOfBasis G1
f2 = rawGBSyzygies G1
assert(f1 * ch1 - gb1 == 0) 
assert(f1 * f2 == 0)

G2 = rawGB(f2,true,-1,{},false,0,algorithm,0,10)
rawStartComputation G2
gb2 = rawGBGetMatrix G2
ch2 = rawGBChangeOfBasis G2
f3 = rawGBSyzygies G2
assert(f2 * ch2 - gb2 == 0) 
assert(f2 * f3 == 0) 
-----------------------------
needs "raw-util.m2"
R2 = rawPolynomialRing(rawZZp(101), lex(x,y,z))
x = rawRingVar(R2,0)
y = rawRingVar(R2,1)
z = rawRingVar(R2,2)
algorithm = 1
G = mat {{x+y+z, x*y+y*z+z*x, x*y*z-1, (x+y+z)^5+x*(x*y*z-1) + 13}}
Gcomp = rawGB(G,true,-1,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp
ch = rawGBChangeOfBasis Gcomp
G * ch
syzm = rawGBSyzygies Gcomp
assert(G * syzm == 0)

Gcomp = rawGB(syzm,true,-1,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
ch = rawGBChangeOfBasis(Gcomp)
g2 = rawGBGetMatrix Gcomp
assert(syzm * ch - g2 == 0)
syz2m = rawGBSyzygies Gcomp
assert(syzm * syz2m == 0)
-----------------------------
needs "raw-util.m2"
R2 = rawPolynomialRing(rawZZp(101), lex(x,y,z))
x = rawRingVar(R2,0)
y = rawRingVar(R2,1)
z = rawRingVar(R2,2)
algorithm = 0
G = mat {{3*x-y^20, 4*y-z^20, x*y-x-1}}
Gcomp = rawGB(G,false,0,{},false,0,algorithm,0,10)
gbTrace = 0
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp
-----------------------------
needs "raw-util.m2"
R1 = polyring(rawZZp(101), (symbol x, symbol y, symbol z, symbol w, symbol t))
algorithm = 0
G = mat {{x-1,y-t^3-2,2*z-t^7-3,5*w-t-7}}
Gcomp = rawGB(G,true,-1,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp
m1 = rawGBSyzygies Gcomp
assert(G * m1 == 0)
Gcomp = rawGB(m1,true,-1,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
m2 = rawGBSyzygies Gcomp
assert(m1 * m2 == 0)

Gcomp = rawGB(m2,true,-1,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
m3 = rawGBSyzygies Gcomp
assert(m2 * m3 == 0)
-----------------------------
needs "raw-util.m2"
algorithm = 0
R1 = rawPolynomialRing(rawZZp(101), elim(1:t,(a,b,c,d)))
t = rawRingVar(R1,0)
a = rawRingVar(R1,1)
b = rawRingVar(R1,2)
c = rawRingVar(R1,3)
d = rawRingVar(R1,4)
G = mat{{b^4-13*a*c, 12*b*c^2-7*a*d^3, t*a-1}}
Gcomp = rawGB(G,false,0,{},false,0,algorithm,0,10)
rawStartComputation Gcomp -- Gröbner basis
m = rawGBGetMatrix Gcomp
<< "check correctness" << endl;
-----------------------------
needs "raw-util.m2"
algorithm = 0
R2 = rawPolynomialRing(rawZZp(101), lex(u,v,x,y,z))
u = rawRingVar(R2,0)
v = rawRingVar(R2,1)
x = rawRingVar(R2,2)
y = rawRingVar(R2,3)
z = rawRingVar(R2,4)
G = mat{{x - 3*u-3*u*v^2+u^3, y-3*v-3*u^2*v+v^3, z-3*u^2+3*v^2}}
Gcomp = rawGB(G,false,0,{},false,0,algorithm,0,10)
--gbTrace = 4
rawStartComputation Gcomp
time m = rawGBGetMatrix Gcomp
rawGBGetLeadTerms(Gcomp,6)
-----------------------------
needs "raw-util.m2"
R1 = polyring(rawZZp(32003), (symbol a .. symbol d))
algorithm = 0
G = mat{{b^4-13*a*c, 12*b*c^2-7*a*b*d^3-1}}
Gcomp = rawGB(G,false,0,{},false,0,algorithm,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp

f = (7*a^2+a+1)*(a^20+a^17+4*a^13+1)^7
g = (7*a^2+a+1)*(6*a^20-123*a^17+4*a^13+1)^6
G = mat{{f,g}}
Gcomp = rawGB(G,false,0,{},false,0,algorithm,0,10)
time rawStartComputation Gcomp -- seems to be in an INFINITE LOOP!! (over QQ)
m = rawGBGetMatrix Gcomp
assert(7*m === mat{{7*a^2+a+1}})
-------------------------------------------
-- Gröbner bases with Schreyer orders ----
-------------------------------------------
needs "raw-util.m2"
R = polyring(rawZZp(101), (symbol a, symbol b, symbol c))
m = mat{{a^2,b^2,a*c}}
F = rawSchreyerSource m
m = rawMatrix1(F,1,(b,a,0_R),0)
G = rawGB(m,false,0,{},false,0,0,0,10)
rawStartComputation G 
rawGBGetLeadTerms(G,10)
<< "DO A MORE SUBSTANTIAL Schreyer order GB" << endl;
--------------------------------------------
-- Gröbner bases using hilbert functions --
--------------------------------------------
needs "raw-util.m2"
gbTrace = 3
R = polyring(rawZZp(101), (symbol x, symbol y, symbol z, symbol w))
G = mat{{(3*x+y+z+w)^4, (7*x+2*y+3*z)^4 + x*w^3, (x+y+z)^4}}
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp;
h = rawHilbert m
-- redo the computation, using this HF.
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
rawGBSetHilbertFunction(Gcomp, h)
rawStartComputation Gcomp -- CRASHES
m1 = rawGBGetMatrix Gcomp;
assert(m == m1)

R1 = polyring2(rawZZp(32003), (symbol x, symbol y, symbol z, symbol w), rawMonomialOrdering { Lex  => 4})
G = mat{{(3*x+y+z+w)^4, (7*x+2*y+3*z)^4 + x*w^3, (x+y+z)^4}}
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
time rawStartComputation Gcomp
m1 = rawGBGetMatrix Gcomp;
assert(rawHilbert m1 == h)
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
rawGBSetHilbertFunction(Gcomp, h)
time rawStartComputation Gcomp
m2 = rawGBGetMatrix Gcomp;
assert(m1 == m2)

------------------------------
-- Gröbner bases over ZZ ----
------------------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol x, symbol y))
f = y^2 - x*(x-12_R)*(x-13_R)
f = y^2 + y - (x^3 - x^2) -- conductor is 11 -- from SLN #476 (Swinnerton-Dyer)
rawConcat(rawDual rawMatrixDiff(rawDual mat{{x,y}}, mat{{f}}), mat{{f}})  -- BUG in diff!!
algorithm = 1
------------------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol x, symbol y))
G = mat{{2*y+1, 3*x^2+2*x, y^2 + y - (x^3 - x^2)}}
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp
assert(m == mat{{53_R, y-26, x-17}})

Gcomp = rawGB(G,true,-1,{},false,0,0,0,10)
rawStartComputation Gcomp
m = rawGBSyzygies Gcomp
Gcomp2 = rawGB(m,true,-1,{},false,0,0,0,10)
rawStartComputation Gcomp2
mgb = rawGBGetMatrix Gcomp2
rawGBGetLeadTerms(Gcomp2,10)
m2 = rawGBSyzygies Gcomp2
Gcomp3 = rawGB(mgb,true,-1,{},false,0,0,0,10)
rawStartComputation Gcomp3
m2 = rawGBSyzygies Gcomp3
------------------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol x, symbol y))
G = mat{{2*y+1, 3*x^2-2*x, y^2 + y - (x^3 - x^2)}}
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp
assert(m == mat{{11_R, y-5, x+3}})
------------------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol x, symbol y))
G = mat{{-3*x^2+50*x-156,  2*y, -x^3+25*x^2+y^2-156*x}}
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
gbTrace = 10
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp
assert(m == mat{{24336_R, 2*y, 4*x-7800, y^2+2*x-3900, x^2+2*x-3900}})
------------------------------     
needs "raw-util.m2"
R = rawPolynomialRing(rawZZ(), lex(symbol y, symbol x))
y = rawRingVar(R,0)
x = rawRingVar(R,1)
G = mat{{-3*x^2+50*x-156,  2*y, -x^3+25*x^2+y^2-156*x}}
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp
assert(m == mat{{24336_R, 4*x-7800, x^2+2*x-3900, 2*y, y^2+2*x-3900}})
------------------------------
needs "raw-util.m2"
R = rawPolynomialRing(rawZZ(), lex(symbol x, symbol y))
y = rawRingVar(R,1)
x = rawRingVar(R,0)
G = mat{{-3*x^2+50*x-156,  2*y, -x^3+25*x^2+y^2-156*x}}
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
rawStartComputation Gcomp
m = rawGBGetMatrix Gcomp
assert(m == mat{{24336_R, 2*y, y^3, 2*x+y^2-3900, x*y^2+12168, x^2+y^2}})

-- The following example fails, probably due to huge numbers
R1 = polyring2(rawZZ(), (symbol x, symbol y, symbol z, symbol w), rawMonomialOrdering { Lex  => 4})
G = mat{{(3*x+y+z+w)^4, (7*x+2*y+3*z)^4 + x*w^3, (x+y+z)^4}}
Gcomp = rawGB(G,false,0,{},false,0,0,0,10)
<< "can we get this GB over ZZ working?" << endl;
--time rawStartComputation Gcomp
--m1 = rawGBGetMatrix Gcomp;
-----------------------------------
-- Gröbner bases over quotients --
-----------------------------------
needs "raw-util.m2"
R = polyring(rawZZp 101, (symbol x, symbol y, symbol z, symbol w))
G = mat{{x*w-y*z, y^2-x*z}}
m = rawgb G
A = rawQuotientRing(R,m)
P = mat{{rawPromote(A,x)}}
rawsyz P

P = mat{{rawPromote(A,x), rawPromote(A,y), rawPromote(A,z), rawPromote(A,w)}}
rawsyz P

m = mat{{x^2+y^2, z^2+w^2}}
A = rawQuotientRing(R,m)
P = mat{{rawPromote(A,x), rawPromote(A,y), rawPromote(A,z), rawPromote(A,w)}}
P1 = rawsyz P
P2 = rawsyz P1 -- (One problem: syzygies are not reduced mod ideal)
print "ERROR: syzygies are not reduced modulo the quotient ideal"
P3 = rawsyz P2
P * P1
P1 * P2
P2 * P3 -- NOT ZERO !! -- BUG BUG -- CRASHES 
P4 = rawsyz P3
P3 * P4 -- NOT ZERO !! -- BUG BUG
-----------------------------------------
-- Gröbner bases over quotients of ZZ --
-----------------------------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol x, symbol y, symbol z, symbol w))
f1 = 36*x*y-5*z^2
f2 = 8*x*y-z
G1 = rawgb mat{{f1,f2}}
A = rawQuotientRing(R, mat{{f1}})
m = mat{{rawPromote(A,f2)}}
G2 = rawgb m
assert(toString G1 == toString G2) -- this doesn't ensure that they are the same...
--------------------------------
-- Gröbner bases over ZZ/p[i]/(i^2+1)
needs "raw-util.m2"
R = polyring(rawZZ(), 1 : symbol i)
A = rawQuotientRing(R, mat{{i^2+1}})
i = rawPromote(A,i)
assert(i^2 == -1)
S = polyring(A,(symbol x, symbol y, symbol z))
  -- BUG: quotient element not re-introduced.

needs "raw-util.m2"
R = polyring(rawZZ(), (symbol i, symbol x))
A = rawQuotientRing(R, mat{{i^2+1}})
i = rawPromote(A,i)
x = rawPromote(A,x)
rawsyz mat{{x^2+1}}
assert(i^2 == -1)
S = polyring(A,(symbol x, symbol y, symbol z))
  -- BUG: quotient element not re-introduced.
  
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test/engine raw4.out"
-- compile-command: "M2 --debug-M2 --stop -e 'input \"raw4.m2\"' -e 'exit 0' "
-- End:
