restart
needsPackage "MinimalPrimes"
  R = QQ[x,r,v,u,b, MonomialOrder=>{Lex=>5}]
  I = ideal(b^3-7*b^2+14*b-7,r^2-u*r+(-2*b^2+9*b-5)*u^2+b^2-4*b,x^2+(b-2)*x*r+r^2+b^2-4*b)
  time minprimes(I, Verbosity=>2)

  R = QQ[b][u][x,r,v, MonomialOrder=>{Lex=>3}]
  I = ideal(b^3-7*b^2+14*b-7,r^2-u*r+(-2*b^2+9*b-5)*u^2+b^2-4*b,x^2+(b-2)*x*r+r^2+b^2-4*b)
  time minprimes(I, Verbosity=>2)

------ issue #156 ---FIXED---------------------------
---Primary decomposition
P = ZZ/2[x0, x1]
I = ideal(0 ); I = promote(I,P);

primaryDecomposition I
--This prints an error: "error: expected a list or sequence of integers or variables in the same ring"

--Running the same command again prints a different error: "error: algorithm missed components!"
primaryDecomposition I

--trimming I returns the first error again.
primaryDecomposition trim I

restart

S = ZZ[a]/(2*a - 1)
R = ZZ[x,y]/(2*x - 1, y-1)

--Both of these commands return "error: can't promote number to ring"
S = minimalPresentation S
R = minimalPresentation R

-----issue #162 --FIXED--------------------------------------------
topCoefficients matrix {{1}}
-- SIGSEGV
-- stack trace, pid 70500:
level 0 -- return addr: 0x0x10471947f -- frame: 0x0x10ae26110
level 1 -- return addr: 0x0x10474425b -- frame: 0x0x10ae26130
level 2 -- return addr: 0x0x107f24da0 -- frame: 0x0x107f22840
-- end stack trace

Process M2 exited abnormally with code 1

-----issue #109 ---FIXED-------------------------------------------
restart
rng = QQ[x]
I = ideal 0_rng
isPrime I
isPrimary I
RI = radical I
isPrime RI
isPrimary RI -- error  NOW WRONG!
primaryDecomposition RI -- error
 
  -- problem: radical RI produces a zero monomial ideal.
  isPrime monomialIdeal (0_rng) -- fails

  M = monomialIdeal (0_rng)
  minimalPrimes M -- fails
  radical M -- ok
  isPrime M -- 
  
--------- issue #83 - FIXED--------------------
A = sub(matrix {{1,2},{0,1}},QQ)
schreyerOrder A -- crashes!

--------- issue #111 --CLOSED works fine now-----------------
restart
rng = QQ[x,y,z];
I = ideal(-(1/2)*x*y^2-x*z+3*x)
pEHV = primaryDecomposition ( I, Strategy=>EisenbudHunekeVasconcelos)
assert (intersect pEHV == I) --ok 

pd := primaryDecomposition I -- modifies something? 
assert (intersect pd == I) --ok 
I --I is still same
"now we get different result for same input: BUG!";

pEHV = primaryDecomposition ( I, Strategy=>EisenbudHunekeVasconcelos) 

assert (intersect pEHV == I) --fails

rng = QQ[x,y,z];
I = ideal(-(1/2)*x*y^2-x*z+3*x)
J = ideal(-x*y^2-2*x*z+6*x)
I==J
decompose J -- correct answer
decompose I -- incorrect answer

needsPackage "MinimalPrimes"
minprimes J
minprimes I

-------- issue #94 --NOT A REAL PROBLEM, commented on------------
needsPackage "FourTiTwo";
A = transpose matrix {{1,0,0},{0,-1,0},{0,0,-1},{1,1,0},{1,0,1},{1,1,1},{-1,-1,-1}}
S = QQ[U1,Um2,Um3,U12,U13,U123,Um1m2m3,Degrees=>entries transpose A];
I = toricGroebner(A,S);
G = gb I;
mingens I
J = ideal(U123*Um1m2m3 - 1, Um3*U123 - U12, Um2*U123 - U13, - U12*U13 + U1*U123)
gb J;
I == J

I_*/degree
ideal I_*
mingens oo

  ----- issue #88 --commented.  Need to fix in engine probably---------------------
A = transpose matrix {{1,0,0},{0,-1,0},{0,0,-1},{1,1,0},{1,0,1},{1,1,1}};
S = QQ[U1,Um2,Um3,U12,U13,U123,Degrees=>entries transpose A];
f = U12*U13-U1*U123;
I = ideal(Um2*U12-U1,Um2*U123-U13,Um3*U13-U1,Um3*U123-U12,U12*U13-U1*U123);
f % I
f // gens I
(gens I) * oo - f
gb I;
f % I

R = QQ[a..d,Degrees=>{{-1,2},{0,3},{-1,5},{0,6}}]
I = ideal(a*b-c, c*d-a*b^3)
isHomogeneous I
gbTrace=3
gens gb(I, DegreeLimit=>6)
---------- issue #132 --CLOSED-------------------
The routine that prints a matrix to the screen seems to fail when the coefficient field was constructed with "toField". Here is an example:

R = (toField (QQ[z]/(z^2+1)))[x]
matrix {{x^2+x+1}}
I could accept the coefficient "1" being printed, but the "+" are missing. Note that printing outside of a matrix works. Also note that when leaving away toField also works:

R = (QQ[z]/(z^2+1))[x]
matrix {{x^2+x+1}}

---------- issue #194 ---FIXED, CLOSED-------------------
R=QQ[x,Inverses=>true,MonomialOrder=>RevLex]
assert not liftable(x-1,QQ)

--------- issue #193 -----------------------
R=QQ[x,Inverses=>true,MonomialOrder=>RevLex]
R/ideal(x-1)  -- hangs

--------- issue #56 --COMMENTED, still open----------------------
A = new MutableMatrix from matrix{{1_RR}}
A = mutableMatrix({{1_RR}}, Dense=>true)
clean(0.1,A) 
A = mutableMatrix({{1_RR}}, Dense=>false)
clean(0.1,A) -- crashes, not implemented.

-------- issue #55 --FIXED, CLOSED---------------------
loadPackage "SimplicialComplexes"
R = QQ[x,y,z,t]
I=monomialIdeal"x2,y2,xz,yt"
 lyubeznikComplex I
 buchbergerComplex I
 -- these are both problems because monomialIdeal{} doesn't return anything useful.
 
 -------- issue #103 ---CLOSED-----------------------------
 rng = QQ[x,y,z]
 I =  ideal(-(1/2)*x*y-3*x,-(1/2)*x*y-3*y)
 C = primaryDecomposition I --sefgault.
 assert(intersect C == I)
 -------
 in Singular:
 LIB("primdec.lib");
 ring rng = 0,(x,y,z),dp;
 ideal I = -(1/2)*x*y-3*x,-(1/2)*x*y-3*y;
 primdecSY(I);

---------- issue #146,149 --------------------------------------
P = ZZ[a, c1, c2, d]

J = ideal(3ac1^2+2a^2c2-6c1^2-11ac2-6c2^2+6a+39c2-38,
    ac1^2d+a^2c2d-2c1^2d-4ac2d-2c2^2d+2ad+15c2d-17d-1,a^2c2^2+3ac1^2+2ac2^2+3c1^2-9ac2+3c2^2+6a-13c2+6,a^3c2+5ac2-13a-13,a^2c1^2-ac1^2-3a^2c2-2ac2^2+2a^2-2c1^2+6ac2-2c2^2-2a+13c2-4,a^3c1^2+2a^3+a^2c2+10c1^2+27ac2+10c2^2-39a-52*c2+7 ); J = promote(J,P);

Jmin = trim J

assert (Jmin == J)
assert (ideal first entries gens Jmin == J)

P = ZZ[a, c1, c2, d]
J1 = ideal(3*a*c1^2+2*a^2*c2-6*c1^2-11*a*c2-6*c2^2+6*a+39*c2-38,a*c1^2*d+a^2*c2*d-2*c1^2*d-4*a*c2*d-2*c2^2*d+2*a*d+15*c2*d-17*d-1,a^2*c2^2+3*a*c1^2+2*a*c2^2+3*c1^2-9*a*c2+3*c2^2+6*a-13*c2+6,a^3*c2+5*a*c2-13*a-13,a^2*c1^2-a*c1^2-3*a^2*c2-2*a*c2^2+2*a^2-2*c1^2+6*a*c2-2*c2^2-2*a+13*c2-4,a^3*c1^2+2*a^3+a^2*c2+10*c1^2+27*a*c2+10*c2^2-39*a-52*c2+7 )
J2 = trim J1
J3 = ideal first entries gens J2
gens J1 % J2
gens J2 % J3
gens J1 % J3
a^2*c2*d % J1 == a^2*c2*d % J3
2*c2*d % J1 == 2*c2*d % J3
leadTerm gens gb J1
leadTerm gens gb J3
