---------------------------------------------------------------------------
-- PURPOSE : Contains examples of computations involving local rings from
--           various sources in the literature.
--           See: https://www.ocf.berkeley.edu/~mahrud/thesis/examples.m2
---------------------------------------------------------------------------

-- I. Examples in the handout:
-- https://www.ocf.berkeley.edu/~mahrud/thesis/handout.pdf

-- Source: https://www.singular.uni-kl.de/Manual/4-0-3/sing_2278.htm#SEC2353
restart
needsPackage "LocalRings"
R = QQ[x,y,z,w];
I = ideal( (x-1)^2 + y^2 - 1) -- Circle of radius one centered at (1,0)
J = ideal"xz-y2,yw-z2,xw-yz"; -- The twisted cubic curve
primaryDecomposition (I+J)
P = radical (primaryDecomposition (I+J))#0
Q = radical (primaryDecomposition (I+J))#1

RP = localRing(R, P);
RQ = localRing(R, Q);

length (RP^1/promote(I+J, RP))
length (RQ^1/promote(I+J, RQ))

use R
K = intersect(I, J)+ideal"z"; -- 4 inhomogeneous generators
C' = res K
C'.dd_1

RP = localRing(R, K+ideal(x-1));
CP = res (R^1/K ** RP)
CP.dd_1
factor (entries liftUp CP.dd_1)_0_1
ideal((x-1)^2+y^2-1) == ideal entries {CP.dd_1}_0_1 -- Indeed, we get back the circle

-- II. Examples in the paper:

-- Example II.3: A Gorenstein ideal of homological dimension 2 with 5 generators
-- Some Structure Theorems for Finite Free Resolutions, Example 5, pp. 127
restart
needsPackage "LocalRings"
R = ZZ/32003[x,y,z];
P = ideal"x,y,z";
RP = localRing(R, P);
I = ideal(x^3+y^3,x^3+z^3,x*y/(z+1),x*z/(y+1),y*z/(x+1));
C = res I;
CP = C ** RP
pruneComplex CP -- FIXME this is surjective, so why isn't the complex complete
assert(liftUp ideal CP.dd_1 == liftUp ideal transpose(CP.dd_3))
-- FIXME can we have f_1=f_3* and f_2=-f_2* ?


-- Example II.9: Is the smooth rational quartic a Cohen-Macaulay curve?
restart
needsPackage "LocalRings";
R = ZZ/32003[a..d];
-- rational quartic curve in P^3:
I = monomialCurveIdeal(R,{1,3,4})
C = res I
M = ideal"a,b,c,d"; -- maximal ideal at the origin
P = ideal"a,b,c";   -- prime ideal
RM = localRing(R, M);
D = C ** RM;
E = pruneComplex D
-- That is to say, the rational quartic curve is not locally Cohen-Macaulay at the origin
-- Therefore the curve is not Cohen-Macaulay
RP = localRing(R, P);
D' = C ** RP;
E' = pruneComplex D'
-- However, the curve is Cohen-Macaulay at the prime ideal P (and in fact any other prime ideal)

-- Example II.11: Computing the syzygy over a local ring using liftUp and pruneDiff
restart
needsPackage "LocalRings";
R = ZZ/32003[vars(0..5)];
I = ideal"abc-def,ab2-cd2-c,-b3+acd";
C = res I;
C = pruneComplex(C, UnitTest => isScalar);
M = ideal"a,b,c,d,e,f";
RM = localRing(R, M);
F = C.dd_2;
FM = F ** RM
--GM = syz FM
 f' = liftUp FM;
 g' = syz f';
 h' = syz g';
 g = g' ** RM;
 h = h' ** RM;
 C = {mutableMatrix g, mutableMatrix h};
 pruneDiff(C, 1);
 toChainComplex C
GM = map(ambient image g, , matrix C#0)
assert(image GM == kernel FM)

-- Intersection Theory: Geometric Multiplicity
restart
needsPackage "LocalRings"
R = ZZ/32003[x,y];
C = ideal"y-x2"; -- parabola
D = ideal"y-1";  -- line
E = ideal"y";    -- line

use R;
P = ideal"y-1,x-1";
RP = localRing(R, P);
assert(length (RP^1/promote(C+D, RP)) == 1)
assert(length (RP^1/promote(C+E, RP)) == 0)

use R;
P = ideal"x,y";  -- origin
RP = localRing(R, P);
assert(length(RP^1/promote(C+D, RP)) == 0)
assert(length(RP^1/promote(C+E, RP)) == 2)


restart
needsPackage "LocalRings"
R = ZZ/32003[x,y];
C = ideal"y-x3";
D = ideal"y-x2";
E = ideal"y";

use R;
P = ideal"x,y";
RP = localRing(R, P);
assert(length(RP^1/promote(C+D, RP)) == 2)
assert(length(RP^1/promote(C+E, RP)) == 3)

use R;
P = ideal"x-1,y-1";
RP = localRing(R, P);
assert(length(RP^1/promote(C+D, RP)) == 1)
assert(length(RP^1/promote(C+E, RP)) == 0)

-- Hilbert-Samuel Function
restart
needsPackage "LocalRings"
R = ZZ/32003[x,y];
RP = localRing(R, ideal gens R);
N = RP^1
q = ideal"x2,y3"
elapsedTime assert({1,2,3,4,5,6} == hilbertSamuelFunction(N, 0, 5)) -- n+1
elapsedTime assert({6,12,18,24,30,36} == hilbertSamuelFunction(q, N, 0, 5)) -- 6(n+1)

-- What is happening around the singularity of the Seepferdchen surface?
-- Idea from the 2017 SIAM Applied Algebraic Geometry Conference in Atlanta
-- see http://wiki.siam.org/siag-ag/index.php/Early_Career_Prize#Award_Type
-- and https://homepage.univie.ac.at/herwig.hauser/gallery.html
restart
needsPackage "LocalRings"
R = QQ[x,y,z]
I = ideal(x^4 - 5/2*x^2*y^3 - x*z^3 + y^6 - y^2*z^3)
assert(isPrime I);
J = ideal jacobian I
L = decompose ideal singularLocus I / radical
-- This is the cute self-intersection singularity:
P = L#0
assert(isPrime P);
RP = localRing(R, P);
M = RP^1/promote(J, RP)
debugLevel = 1
-- The multiplicity of the singularity of the Seepferdchen surface is 45
--time length(M) -- this is too slow, so we break it up:
time A = hilbertSamuelFunction(M, 0, 5)  -- 1 3 6 8 8 6 -- 1.2 seconds
time B = hilbertSamuelFunction(M, 6, 10) -- 5 4 3 1 0 -- 1.2 seconds
sum A + sum B

M = RP^1/promote(I, RP)
time A = hilbertSamuelFunction(M, 0, 4)  -- 1 3 6 8 8 6 -- 1.2 seconds
time A = hilbertSamuelFunction(M, 5, 5)  -- 1 3 6 8 8 6 -- 1.2 seconds
time A = hilbertSamuelFunction(M, 6, 6)  -- 1 3 6 8 8 6 -- 1.2 seconds
time B = hilbertSamuelFunction(M, 7, 7) -- 5 4 3 1 0 -- 1.2 seconds
time B = hilbertSamuelFunction(M, 8, 8) -- 5 4 3 1 0 -- 1.2 seconds
time B = hilbertSamuelFunction(M, 9, 9) -- 5 4 3 1 0 -- 1.2 seconds
time B = hilbertSamuelFunction(M, 10, 10) -- 5 4 3 1 0 -- 1.2 seconds

-- Computations in Algebraic Geometry with Macaulay2 pp 61
restart
needsPackage "LocalRings"
R = QQ[x,y,z];
RP = localRing(R, ideal gens R);
I = ideal"x5+y3+z3,x3+y5+z3,x3+y3+z5"
M = RP^1/I
elapsedTime assert(length(RP^1/I) == 27) -- 0.5 seconds
elapsedTime assert((hilbertSamuelFunction(M, 0, 6))//sum == 27) -- 0.54 seconds
elapsedTime assert((hilbertSamuelFunction(max ring M, M, 0, 6))//sum == 27) -- 0.55 seconds
-- alternate method to get these numbers
R = QQ[h,x,y,z,MonomialOrder=>Eliminate 1];
I = ideal homogenize(matrix "x5+y3+z3,x3+y5+z3,x3+y3+z5",h)
elapsedTime degree monomialIdeal sub(leadTerm gens gb I, h=>1)
elapsedTime reduceHilbert hilbertSeries monomialIdeal sub(leadTerm gens gb I, h=>1)

--TODO Computations in Algebraic Geometry with Macaulay2 pp 26-27


-- Test from Mengyuan Zhang
restart
needsPackage "LocalRings"
R = ZZ/32003[x,y,z,w];
P = ideal "  yw-z2,   xw-yz,  xz-y2"
I = ideal "z(yw-z2)-w(xw-yz), xz-y2"
codim I == codim P --Hence this is finite, thus I is artinian in R_P, i.e. RP/IP is an artinian ring.
radical I == P

RP = localRing(R, P)
N = RP^1/promote(I, RP)
assert(length(N) == 2)
assert(length(N) == degree I / degree P)

assert({1,1,0,0} == for i from 0 to 3 list hilbertSamuelFunction(N, i))
assert({1,1,0,0} == for i from 0 to 3 list hilbertSamuelFunction(max RP, N, i))

------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------

-- III. Additional examples:

restart
debug needsPackage "PruneComplex"
needsPackage "LocalRings"
R = ZZ/32003[a..f]
I = ideal"abc-def,ab2-cd2-c,acd-b3-1"
C = freeRes I
D = pruneComplex(C, UnitTest => isScalar, Strategy => null, Direction => "both")
--Not necessarily minimal!!
C.dd
D.dd

-- 1506.06480.pdf Ex. 3.2
restart
needsPackage "LocalRings"
R = ZZ/32003[x,y,z]
P = ideal"x,y,z"
RP = localRing(R, P) -- really we want R=kk[[x,y,z]] with infinite kk though
I = ideal"x2y,y2z,z2x,xyz"
Q = ideal"x2y,y2z,z2x"
                     -- Q is a minimal reduction of I with red_Q(I)=2
C = res I            -- presentation of I
C = pruneComplex C   -- just to make sure it's minimal
C.dd


restart
needsPackage "LocalRings";
S = ZZ/32003[t]
R = ZZ/32003[x,y]
f = map(S, R, {t^4, t^6 + t^7})
I = kernel f
C = res I
C.dd
M = ideal"x,y"; -- maximal ideal at the origin
P = ideal"x";   -- prime ideal
RM = localRing(R, M);
D = C ** RM;
E = pruneComplex(D, UnitTest => isUnit)
E.dd#1
RP = localRing(R, P);
D' = C ** RP;
E' = pruneComplex(D', UnitTest => isUnit)


-- Wolfram Decker, Computations in Intersection Theory (slides)
restart
needsPackage "LocalRings"
R = ZZ/32003[x,y]
I = ideal"y2-x3"
J = ideal"x2-y3" -- transversal cusps
K = ideal"2y2-x3" -- tangential cusps

RM = localRing(R, ideal gens R)

length (RM^1/promote(I+J, RM)) -- i(V(I),V(J); m) = 4
length (RM^1/promote(I+K, RM)) -- i(V(I),V(K); m) = 6

------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------

-- IV. Under development:

-- Spectrahedral Sun

restart
debugLevel = 1
needsPackage "LocalRings";
R = ZZ/32003[x,y,z,w]
I = ideal"8(-x6-y6-z6+x4y2+x4z2+x2y4+x2z4+y4z2+y2z4-10x2y2z2+3(x4+y4+z4)+2(x2y2+x2z2+y2z2)-3(x2+y2+z2)+1)"
assert(isPrime I);
J = ideal jacobian I
L = decompose ideal singularLocus I

P = L#0
RP = localRing(R, P);
M = RP^1/promote(J, RP);

time A = hilbertSamuelFunction(M, 0, 2)

for P in L list (
    RP = localRing(R, P);
    M = RP^1/promote(J, RP);
    length(M, Strategy=>Hilbert)
    )


-- 1806.07408 Ex. 1.4
restart
needsPackage "LocalRings"
R = QQ[x,y,z,w];
I = ideal"xz-y2,yw-z2,xw-yz"; -- The twisted cubic curve
J = ideal"xz-y2,z(yw-z2)-w(xw-yz)";
RP = localRing(R, I)

M = RP^1/promote(J, RP)
length M -- 2


-- TODO Gorenstein Cover of 1-dim Domain
restart
needsPackage "LocalRings"
needsPackage "ReesAlgebra"
S = ZZ/32003[vars(0..3)]
I = monomialCurveIdeal(S, {3, 4, 5})
r = codim I
SP = localRing(S, ideal gens S)
IP = promote(I, SP)
R = S/I
M = S^1/I
W = Ext^r(M, S^1)
f = map(R, S)
W' = coker f presentation W
R' = reesAlgebra W'
describe R'
(R', phi) = flattenRing R'
describe R'
R'' = R'^1 / module promote(ideal "1-x", R')
R'' = R' / promote(ideal "1-a", R')
describe R''
ideal R''

-- TODO Hilbert-Samuel Polynomial
restart
needsPackage "LocalRings"
R = ZZ/32003[x,y,z,w]
-- Rational quartic, see Eisenbud Ex. 12.4
I = monomialCurveIdeal(R, {1, 3, 4})
RM = localRing(R, ideal gens R)

q = ideal"x,w"
N = RM^1/promote(I, RM)

for i from 0 to 3 list hilbertSamuelFunction(q, N, i)
-- increasing linearly so degree is 1,
-- hence the difference is the leading coefficient of the hilbert-samuel polynomial
-- which is 4.

-- TODO Geometric multiplicity != Algebraic multiplicity
restart
needsPackage "LocalRings"
R = ZZ/32003[x,y]
I = ideal"x2,xy,y2"
RM = localRing(R, ideal gens R)

N = RM^1/promote(I, RM)
length(N) -- geometric multiplicity = 3, also computed by sum(HSF(N, i))

for i from 0 to 3 list hilbertSamuelFunction(promote(I, RM), RM^1, i)
-- algebraic multiplicity = 4, is the coefficient of HSP.

-- TODO Smooth curves and differentials!!

-- TODO blow-ups and resolution of singularities using reesAlgebra
restart
needsPackage "ReesAlgebra"
needsPackage "LocalRings"
S = ZZ/32003[x,y,z]
I = ideal "y2z-x3-x2z"
T = S/I
J = ideal"x,y"
R' = reesAlgebra J
(R, phi) = flattenRing R'
describe R
decompose ideal R
-- FIXME
RP = localRing(R, promote(ideal"x,y,z", R))
mingens R^1

-- Projective Resolutions of Cohen-Macaulay Algebras
-- 1981-002.pdf Ex. 4.2
restart
needsPackage "LocalRings"
k = ZZ/32003 -- really wanted CC
S = k[s,t, x,y,z]
SP = localRing(S, ideal gens S)
I = ideal"x2-st, y2-s(s2+t2), z2-t(s2+t2), xy-sz, xz-ty, yz-(s2+t2)x"
C = res I
C.dd
-- TODO do a blow up of this

--Syzygies of Multiplier Ideals on Singular Varieties
-- 0804.4188.pdf Ex. 3.1
restart
needsPackage "LocalRings"
R = ZZ/32003[x,y,z]
RP = localRing(R, ideal gens R)
n = 3
I = ideal(x^n+y^n+z^n)
res I
oo.dd
-- TODO blow this up

-- Algebra Structures for Finite Free Resolution, and Some Structure Theorems for Ideals of Codimension 3
-- Buchsbaum, Eisenbud
-- 1977-002.pdf, pp 476
restart
needsPackage "LocalRings"
R = ZZ/32003[w,x,y,z]
RP = localRing(R, ideal gens R)
I = ideal"x2-wy,yz-w3,z2-wxy,xy2-w2z,y3-wxz"
-- 5-generator perfect ideal of grade 3, with type 3
-- kernel of R->k[[t]] obtained from {7,8,9,12}
C = res I
C.dd
f3 = C.dd_3
minors(2, f3)
--this doesn't contain four elements from a minimal generating set for I
--hence I is not of the form (I_0, x)

-- FIXME: A TEST LIKE ABOVE FOR HIGHER DIMENSION
-- TODO:  Do we need to use Serre's alternating sums? See
restart
needsPackage "LocalRings"
R = ZZ/32003[x,y,z]
C = ideal"x2+y2-z" -- paraboloid
D = ideal"x2+y2-2" -- circle
E = ideal"x,y"     -- line
F = ideal"z"       -- plane

use R
P = ideal"x,y,z"   -- origin
RP = localRing(R, P)
length(RP^1/promote(C+D, RP)) == 0
length(RP^1/promote(C+E, RP)) == 1 -- FIXME
length(RP^1/promote(C+F, RP)) == 2 -- FIXME

use R
P = ideal"x-1,y-1"
RP = localRing(R, P)
length(RP^1/promote(C+D, RP)) == 1 -- FIXME
length(RP^1/promote(C+E, RP)) == 0
length(RP^1/promote(C+F, RP)) == 0


-- 1403.3599.pdf Ex 3.14
-- r(R) = l_R(Ext_R^d(R/m, R)) -- Cohen-Macaulay type of R??
restart
needsPackage "LocalRings"
n = 4
S = ZZ/32003[x_1..x_n,  dx_1..dx_n]
A = matrix({{x_1..x_n},{dx_1..dx_n}})
I = minors(2,A)
R = S/I
P = ideal(x_1..x_n,dx_1..dx_n)
d = dim R - n -- dim RP = n+1, so d=1
Ext^(n)(R^1/P, R^1)
length oo -- FIXME this should be n-1=3
RP = localRing(R, P)
M = promote(P, RP)
-- Then R/(X_i-Y_(i-1) : 0<1<n+1)R  \cong k[[X_1..X_n]]/minor(2,({{X1..Xn},{X2..Xn,X1}}))
-- this is a cohen macaulay local ring of dim 1, also almost gorenstein local rings


-- TODO
-- 1403.3599.pdf Ex 7.6
restart
needsPackage "LocalRings"
S = ZZ/32003[x,y]
R = kk[x4,x3y,x2y2,xy3,y4]


-- 1403.3599.pdf Ex 7.11
restart
needsPackage "LocalRings"
V = ZZ/32003[t]
S = ZZ/32003[x,y,z,w]
F = map(V, S,{t^5,t^6,t^7,t^9})
f = matrix F
g = gens ker F
h = syz g
i = syz h
j = syz i
C = {mutableMatrix g, mutableMatrix h, mutableMatrix i, mutableMatrix j}
(C1, P1) = pruneDiff(C, 0, PruningMap => true)
(C2, P2) = pruneDiff(C, 1, PruningMap => P1)
(C3, P3) = pruneDiff(C, 2, PruningMap => P2)
(C4, P4) = pruneDiff(C, 3, PruningMap => P3)
C' = chainComplex for M in C4 list  map(S^(numrows M), S^(numcols M), matrix M)
C'.dd

VP = localRing(V, ideal"t")
SP = localRing(S, ideal"x,y,z,w")
FP = map(VP, SP,{t^5,t^6,t^7,t^9})
fp = matrix FP
gp = g ** SP
hp = syz gp
ip = syz hp
jp = syz ip
CP = {mutableMatrix gp, mutableMatrix hp, mutableMatrix ip, mutableMatrix jp}
(C1, P1) = pruneDiff(CP, 0, PruningMap => true)
(C2, P2) = pruneDiff(CP, 1, PruningMap => P1)
(C3, P3) = pruneDiff(CP, 2, PruningMap => P2)
(C4, P4) = pruneDiff(CP, 3, PruningMap => P3)
CP' = chainComplex for M in C4 list  map(SP^(numrows M), SP^(numcols M), matrix M)
CP'.dd


-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
-- V. Exercises from Commutative Algebra with a view towards algebraic geometry

restart
needsPackage "LocalRings"
-- Eisenbud 12.2 exercises
R = ZZ/32003[x,y,z]
RP = localRing(R, ideal gens R)
M = ideal"x,y,z"
A = ideal"y,z"
B = ideal"y2,z2"
C = A^2
-- Show these are parameter ideals of R/(f) where f is homogeneous form of degree d, monic in x
-- Compute HS functions


-- TODO Quotient
-- Eisenbud 12.2 exercises
exit
make -j4 all-in-d all-in-e all-in-bin
../M2
restart
debug Core
debug needsPackage "LocalRings"
R = ZZ/32003[x,y,z,w]
RP = localRing(R, ideal gens R)
M = matrix{{x,y,z},{y,z,w}}
I = minors(2, M)
SP = RP/I
q = ideal"x,w"
-- TODO Show that q is parameter ideal of R?
isSubset(m^n, q)
-- FIXME Compute HS function with respect to q, for which module? RP^1? RP^1/q?
for i from 0 to 3 do << hilbertSamuelFunction(q, RP^1/q, i) << endl
-- TODO Compute HS polynomial of it
-- Compute length of RP/q
N = RP^1/q
for i from 0 to 3 do << hilbertSamuelFunction(N, i) << endl
length N
-- TODO IS THIS CORRECT?


-- there are more examples in Section 12.2

-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
restart
needsPackage "LocalRings"
needsPackage "PruneComplex"
debugLevel=1
debug PruneComplex
debug LocalRings
elapsedTime check PruneComplex -- 17
elapsedTime check LocalRings -- 15

uninstallPackage "PruneComplex"
uninstallPackage "LocalRings"

installPackage "PruneComplex"
installPackage "LocalRings"

-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
-- Local Rings in Engine
exit
make -j4 all-in-d all-in-e all-in-bin
../M2

restart
debug Core
debug needsPackage "PruneComplex"
debug needsPackage "LocalRings"
  R = ZZ/32003[vars(0..3)]
  I = monomialCurveIdeal(R, {1, 3, 4})
  C = res I
  P = ideal"a,b,c";
  RP = localRing(R, P);
  J = ideal(gens I ** RP)
  D = res J

-- TODO look at rawMatrixRowScale(raw D_1,raw f,2,false)
-- TODO first do isScalar, then do isUnit
-- FIXME is the size of output of enginePruneComplex changing?
-- TODO compute filtration degree for local rings
-- TODO deal with degrees in the engine

----------
-- This shouldn't crash, it should give an error.
restart
debug Core
debug needsPackage "LocalRings"
  R = ZZ/32003[vars(0..4)]
  P = ideal"a,b,c,d";
  RP = localRing(R, P);
  f = transpose matrix{{a, 0, c}, {0, b, 0}}
  g = transpose matrix{{a, b, c}}
  G := syz(f | g)
  toChainComplex {f|g, G}
  pruneComplex {G, f|g}
