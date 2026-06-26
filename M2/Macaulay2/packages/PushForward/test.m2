--test 0
TEST ///
kk=ZZ/32003
R4=kk[a..d]
R5=kk[a..e]
R6=kk[a..f]
M=coker genericMatrix(R6,a,2,3)
pdim M

G=map(R6,R5,{a+b+c+d+e+f,b,c,d,e})
F=map(R5,R4,random(R5^1,R5^{4:-1}))

P=pushFwd(G,M)
assert (pdim P==1)

Q=pushFwd(F,P)
assert (pdim Q==0)
///

-- test 1
TEST ///
P3=QQ[a..d]
M=comodule monomialCurveIdeal(P3,{1,2,3})

P2=QQ[a,b,c]
F=map(P3,P2,random(P3^1,P3^{-1,-1,-1}))
N=pushFwd(F,M)

assert(hilbertPolynomial M==hilbertPolynomial N)
///

-- test 2
TEST ///
kk = QQ
R = kk[x,y]/(x^2-y^3-y^5)
R' = integralClosure R
pr = pushFwd map(R',R)
q = pr_0 / (pr_0)_0
use R
assert(ann q==ideal(x,y))
assert isModuleFinite map(R', R)
///

-- test 3
TEST ///
kkk=ZZ/23
kk=frac(kkk[u])
T=kk[t]
x=symbol x
PR=kk[x_0,x_1]
R=PR/kernel map(T,PR,{t^3-1,t^4-t})
PS=kk[x_0,x_1,x_2]
S=PS/kernel map(T,PS,{t^3-1,t^4-t,t^5-t^2})

rs=map(S,R,{x_0,x_1})
st=map(T,S,{t^3-1,t^4-t,t^5-t^2})
assert isModuleFinite rs
assert isModuleFinite st
pst=pushFwd st

MT=pst_0
k=numgens MT

un=transpose matrix{{1_S,(k-1):0}}
MT2=MT**MT

mtt2=map(MT2,MT,un**id_MT-id_MT**un)
MMS=kernel mtt2

r1=trim minimalPresentation kernel pushFwd(rs,mtt2)
r2=trim minimalPresentation pushFwd(rs,MMS)
r3=trim (pushFwd rs)_0

assert(r1==r2)
assert(flatten entries relations r2 == flatten entries relations r3)
///

-- test 4
TEST ///
kk=ZZ/3
T=frac(kk[t])
A=T[x,y]/(x^2-t*y)

R=A[p]/(p^3-t^2*x^2)
S=A[q]/(t^3*(q-1)^6-t^2*x^2)
f=map(S,R,{t*(q-1)^2})
assert isModuleFinite f
pushFwd f

p=symbol p
R=A[p_1,p_2]/(p_1^3-t*p_2^2)
S=A[q]
f=map(S,R,{t*q^2,t*q^3})
assert isModuleFinite f
pushFwd f

i=ideal(q^2-t*x,q*x*y-t)
p=pushFwd(f,i/i^3)
assert(numgens p==2)
///

-- test 5
TEST ///
kk=QQ
A=kk[x]
B=kk[y]/(y^2)
f=map(B,A,{y})
d=map(B^1,B^1,matrix{{y^2}})
assert isModuleFinite f
pushFwd f
use B
d=map(B^1,B^1,matrix{{y^2}})
assert(pushFwd(f,d)==0)
///

-- test 6
TEST ///
kk=QQ
A=kk[t]
B=kk[x,y]/(x*y)
use B
i=ideal(x)
f=map(B,A,{x})
assert not isModuleFinite f
assert(isFreeModule pushFwd(f,module i))
///

-- test 7
TEST ///
kk=ZZ/101
n=2

PA=kk[x_1..x_(2*n)]
iA=ideal apply(toList(1..n),i->(x_(2*i-1)^i-x_(2*i)^(i+1)))
A=PA/iA

PB=kk[y_1..y_(2*n-1)]
l=apply(toList(1..(2*n-1)),i->(x_i+x_(i+1)))
g=map(A,PB,l)
time iB=kernel g;
B=PB/iB

f=map(A,B,l)
assert isModuleFinite f
assert isModuleFinite g
time h1=pushFwd g;
ph1=cokernel promote(relations h1_0,B);
time h2=pushFwd f;

assert(ph1==h2_0)
///

--test 8
TEST ///
A = QQ
B = QQ[x]/(x^2)
N = B^1 ++ (B^1/(x))
f = map(B,A)
assert isModuleFinite f
pN = pushFwd(f,N, MinimalGenerators => true)
assert(isFreeModule pN)
assert(numgens pN == 3)
///

--test 9
TEST///
  debug needsPackage "PushForward"
  kk = ZZ/101
  A = kk[s]
  B = A[t]
  C = B[u]
  f = map(C,B)
  g = map(C,B,{t})
  assert(isInclusionOfCoefficientRing f)
  assert(isInclusionOfCoefficientRing g)

  kk = ZZ/101
  A = frac (kk[s])
  B = A[t]
  C = B[u]
  f = map(C,B)
  g = map(C,B,{t})
  assert(isInclusionOfCoefficientRing f)
  assert(isInclusionOfCoefficientRing g)
///

--test 10
TEST///
  debug  needsPackage "PushForward"
  s = symbol s; t = symbol t
  kk = ZZ/101
  A = kk[s,t]
  -- note: this ideal is NOT the rational quartic, and in fact has an annihilator over A.
  L = A[symbol b, symbol c, Join => false]/(b*c-s*t, t*b^2-s*c^2, b^3-s*c^2, c^3 - t*b^2)
  isHomogeneous L
  describe L
  basis(L, Variables => L_*)
  inc = map(L, A)
  assert isInclusionOfCoefficientRing inc
  assert isModuleFinite L
  assert isModuleFinite inc
  (M,B,pf) = pushFwd inc
  assert( B*presentation M  == 0)
  assert(numcols B == 5)
///

--test 11
TEST///
  debug  needsPackage "PushForward"
  s = symbol s; t = symbol t
  kk = ZZ/101
  A = kk[s,t]
  L = A[symbol b, symbol c, Join => false]/(b*c-s*t,c^3-b*t^2,s*c^2-b^2*t,b^3-s^2*c)
  isHomogeneous L
  describe L
  basis(L, Variables => L_*)
  inc = map(L, A)
  assert isInclusionOfCoefficientRing inc
  assert isModuleFinite L
  assert isModuleFinite inc
  (M,B,pf) = pushFwd inc -- ok.  this works, but isn't awesome, as it uses a graph ideal.
  assert( B*presentation M  == 0)
  assert(numcols B == 5)
///

--test 12
TEST///
  debug  needsPackage "PushForward"
  s = symbol s; t = symbol t
  kk = ZZ/101
  L = kk[s, symbol b, symbol c, t]/(b*c-s*t, t*b^2-s*c^2, b^3-s*c^2, c^3 - t*b^2)
  A = kk[s,t]
  isHomogeneous L
  inc = map(L, A)
  (M,B,pf) = pushFwd inc
  assert( B * inc presentation M  == 0)
  assert(numcols B == 5)
  pushForward(inc, L^1)
///

--test 13
TEST///
  kk = QQ
  A = kk[x]
  R = A[y, Join=> false]/(y^7-x^3-x^2)
  (M,B,pf) = pushFwd map(R,A)
  pushFwd matrix{{y}}
  (M1,B1,pf1) = pushFwd R
  assert(pushFwd(R^3) == pushFwd(map(R,A), R^3))
  -- this asserts among other things that the degrees on source B and source B1
  -- are the same which required some fussing to get right
  assert((M1,B1) == (M,B))
  assert(pushFwd matrix{{y}} == pushFwd(map(R,A),matrix{{y}}))
  assert(isFreeModule M and rank M == 7)
  assert(B == basis(R, Variables => R_*))
  assert( pf(y+x)- matrix {{x}, {1}, {0}, {0}, {0}, {0}, {0}} == 0)
  R' = integralClosure R
  (M,B,pf) = pushFwd map(R',R)
  use R
  assert(M == cokernel(map(R^2,R^{{-6}, {-4}},{{-x^2-x,y^4}, {y^3,-x}})))
  assert(pf w_(2,0) - matrix {{0}, {1}} == 0)
///

--test 14
TEST ///
  -- long running test ~2s
  kk = QQ
  A = kk[x, DegreeRank => 0]
  R = A[y,z, Join => false]
  I = ideal(y^4-x*y-(x^2+1)*z^2, z^4 - (x-1)*y-z^2 - z - y^3)
  B = R/I
  assert isModuleFinite map(B,A)
  (M,g,pf) = pushFwd B
  pushFwd B^1
  pushFwd B^{1}
  fy = pushFwd matrix{{y}}
  fz = pushFwd matrix{{z}}
  assert(fy*fz == pushFwd matrix{{y*z}})
  inc = map(B,A)
  pushFwd(inc, B^1)
  pushFwd(inc, B^{1})
  fy = pushFwd(inc, matrix{{y}})
  fz = pushFwd(inc, matrix{{z}})
  assert(fy*fz == pushFwd(inc, matrix{{y*z}}))

  kk = QQ
  A = kk[x]
  R = A[y,z, Join => false]
  I = ideal(y^4-x*y-(x^2+1)*z^2, z^4 - (x-1)*y-z^2 - z - y^3)
  B = R/I
  assert isModuleFinite map(B,A)
  (M,g,pf) = pushFwd B
  pushFwd B^1
  pushFwd B^{1}
  fy = pushFwd matrix{{y}}
  fz = pushFwd matrix{{z}}
  assert(fy*fz == pushFwd matrix{{y*z}}) -- false
  assert(fy*fz - pushFwd matrix{{y*z}} == 0)
  inc = map(B,A)
  pushFwd(inc, B^1)
  pushFwd(inc, B^{1})
  fy = pushFwd(inc, matrix{{y}})
  fz = pushFwd(inc, matrix{{z}})
  assert(fy*fz == pushFwd(inc, matrix{{y*z}}))

  kk = QQ
  A = kk[x, DegreeRank => 0]
  R = A[y,z]
  I = ideal(y^4-x*y-(x^2+1)*z^2, z^4 - (x-1)*y-z^2 - z - y^3)
  B = R/I
  assert isModuleFinite map(B,A)
  (M,g,pf) = pushFwd B
  pushFwd B^1
  pushFwd B^{1}
  fy = pushFwd matrix{{y}}
  fz = pushFwd matrix{{z}}
  fy*fz == pushFwd matrix{{y*z}}

  kk = QQ
  A = kk[x]
  R = A[y,z]
  I = ideal(y^4-x*y-(x^2+1)*z^2, z^4 - (x-1)*y-z^2 - z - y^3)
  B = R/I
  assert isModuleFinite map(B,A)
  (M,g,pf) = pushFwd B
  pushFwd B^1
  pushFwd B^{{0,1}}
  fy = pushFwd matrix{{y}} -- good
  fz = pushFwd matrix{{z}}
  assert(fy*fz == pushFwd matrix{{y*z}}) -- good
  assert(fy*fz - pushFwd matrix{{y*z}} == 0)
///

--test 15
TEST ///
  n = 4
  d = 4
  c = 2
  kk = ZZ/32003;
  S = kk[x_1..x_n];
  I = ideal random(S^1, S^{c:-d});
  R = S/I;
  A = kk[t_1..t_(n-c)];
  phi = map(R, A, random(R^1, R^{n-c:-1}));
  elapsedTime assert isModuleFinite phi
  elapsedTime M1 = pushFwd(phi, R^1)
  elapsedTime M2 = pushForward(phi, R^1);
  assert(M1 == M2)
///

--test 16
TEST ///
-- tower of rings is module finite over the base even if intermediate steps are not
kk = ZZ/101
R = kk[a..b]

S = R/a^2
assert(not isModuleFinite(S))

T = S/b^2
assert(isModuleFinite(T))
///

--test 17
TEST ///
-- quotient of a moduleFinite ring is moduleFinite
kk = ZZ/101
R = kk[a]/a^2
S = R[b,c]/ideal {b^2, c^2}
assert(isModuleFinite(S))

I = ideal {a^2 - b*c}
S' = S/I
assert(isModuleFinite(S'))
///

--test 18
TEST ///
-- pure skew commutative rings are module finite over their coefficient rings
kk = ZZ/101
R = kk[a..c, SkewCommutative => true]
assert(isModuleFinite(R))
///

--test 19
TEST ///
-- mixed skew commutative rings are not module finite unless we quotient by an
-- ideal whose support contains the commuting variables
kk = ZZ/101
R = kk[a..d, SkewCommutative => {c..d}]
assert(not isModuleFinite(R))

S = R / ideal {a^2, b^2 - a}
assert(isModuleFinite(S))
///

--test 20
TEST ///
kk = ZZ/101
R = kk[a]/ideal a^3
S' = R[b,c]
S = S' / ideal {a^2, b^2, c^2}
(rS, rB, pf) = pushFwd S

-- rS and S are isomorphic over R hence have the same kk dimension
assert(degree rS === degree S)

-- rS is an R-module
assert(ring rS === R)

-- rS is not free over R since multiplication by a^2 kills it
assert(not isFreeModule rS)

assert(pf(a_S * rB) - a * pf(rB) == 0)
assert(a^2 * pf(rB) == 0)
///

-- test 21
TEST ///
-- pushforward and pushforward' are inverse - submodule case
-- degrees are screwed up observe weird asserts.
kk = ZZ/101
R = kk[a];
S = R[b, Join => false]/ ideal {a^2, b^3};
I = ideal {a, b};
N = directSum(module I, module I^2);
ns = N_{0..numgens N - 1}

o = new OptionTable from {MinimalGenerators => true}
M = pushFwd(N, o)
assert(ns - pushforward' pushforward(M, ns) == 0)

M = pushFwd(N)
assert(ns - pushforward' pushforward(M, ns) == 0)
///

-- test 22
TEST ///
-- pushforward and pushforward' are inverse - subquotient case
kk = ZZ/101
R = kk[a];
S = R[b, Join => false];
I = ideal {a^2, b^2};
N = directSum(I/I^2, I^2/I^3);
ns = N_{0..numgens N - 1}

o = new OptionTable from {MinimalGenerators => true}
M = pushFwd(N, o)
assert(ns == pushforward' pushforward(M, ns))

-- with pruning (default)
M = pushFwd(N)
assert(ns == pushforward' pushforward(M, ns))
///

-- test 23
TEST ///
kk = ZZ/101
S = kk[a..c, SkewCommutative => true];
F = S^2;
x = random(1, F);
N = cokernel matrix {x};
ns = N_{0..numgens N - 1}

o = new OptionTable from {MinimalGenerators => true}
M = pushFwd(N, o)
assert(ns == pushforward' pushforward(M, ns))

-- with pruning (default)
M = pushFwd(N)
assert(ns == pushforward' pushforward(M, ns))
///

-- test 24
TEST ///
-- another skew commutative case that triggered a bug fix or two

kk = ZZ/101
S = kk[a..e, SkewCommutative => true];
I = cokernel matrix {{a*b + c, d}}
I' = pushFwd I
ms = I'_{0..numgens I' - 1}
assert(ms == pushforward(I', pushforward' ms))
///

-- test 25
TEST ///
-- pushFwd of 1x1 matrix is compatible with pushFwd of elements in source
kk = ZZ/101
R = kk[a]
S = R[b, Join => false] / ideal {a^2, b^3}

-- make a map from S^1 to S^1 and push it forward to R
s = 1_S + 2_S*a + 3_S*b + 4_S*a*b
F = map(S^1, S^1, matrix s)
pF = pushFwd F

-- get pushFwd of S^1 to an R-module and check it has the right source and target
rS = first pushFwd S

assert(source pF == rS)
assert(target pF == rS)

-- diagram commutes:
-- Hom(M, M) x M ---> Hom(pM, pM) x pM
--      |                   |
--      V                   V
--      M        --->       pM
ms = pushforward' rS_{0..numgens rS - 1}
assert(pF * (pushforward(rS, ms)) - pushforward(rS, F * ms) == 0)
///

-- test 26
TEST ///
-- pushFwd of higher rank matrix is compatible with pushFwd / back of module elements
kk = ZZ/101
R = kk[a]
S = R[b, Join => false] / ideal {a^2, b^3}

-- make a map from S^3 to S^3 and push it forward to R
N = S^3
F = random(N, N)
pF = pushFwd F
M = pushFwd N

ns = matrix N_{0..numgens N - 1}

assert(pushforward(M, F * ns) == pF * pushforward(M, ns))
///

-- test 27
TEST ///
-- maps back and forth work for matrices not just columns
kk = ZZ/101
R = kk[a..c]
I = ideal vars R
N = I^3/I^5
M = pushFwd(N, MinimalGenerators => true)

ns = N_{0..numgens N - 1}
assert(ns == pushforward' pushforward(M, ns))

ms = M_{0..numgens M - 1}
-- puzzle: this equality fails if MinimalGenerators => true option is not used
assert(ms == pushforward(M, pushforward' ms))
///

-- test 28
TEST ///
-- test various interfaces for pushforward and pushforward' methods
kk = ZZ/101
R = kk[a..c]
f = map(R, kk)

I = ideal vars R
N = I/I^2
M = pushFwd N

-- test elements to push around
n = N_{0}
m = M_{0}

-- check vector and matrix interface for pushforward
assert(n == pushforward' pushforward(M, n))
assert(n == pushforward' pushforward(M, vector n))
assert(n == pushforward' pushforward(f, n))
assert(n == pushforward' pushforward(f, vector n))

-- check vector and matrix interface for pushforward'
assert(m == pushforward(M, pushforward' m))
assert(m == pushforward(M, pushforward' vector m))
assert(m == pushforward(f, pushforward' m))
assert(m == pushforward(f, pushforward' vector m))

-- check vector and matrix interface for pushforward with explicit f
S = kk[x]
f = map(R, S, {a})
M' = pushFwd(f, N)
assert(n == pushforward' pushforward(f, n))
assert(n == pushforward' pushforward(f, vector n))
///

-- test 29
TEST ///
-- pushforward interface for ring elements
kk = ZZ/101
R = kk[a..c]/ideal {a^2, b^3, c^5}
r = a + b + c

-- check pushforward interface with explicit map
S = kk[x]
f = map(R, S, {a})
pushFwd(f)
assert(r * 1_R^1 == pushforward' pushforward(f, r))
///

-- test 30
TEST ///
-- require explicit module in multiple pushforward case
kk = ZZ/101
R = kk[a,b] / ideal {a^2 + 1, b^3 + a^2*b + 2}
f = map(R, kk)
M = first pushFwd R

-- specifying ring map works
assert(matrix a + b == pushforward' pushforward(f, a + b))
M' = first pushFwd(R, MinimalGenerators => false)

-- specifying explicit module
assert(matrix {{a + b}} == pushforward' pushforward(M, a + b))
assert(matrix {{a + b}} == pushforward' pushforward(M', a + b))
///

-- test 31
TEST ///
-- a simple edge cases around pushFwd of explicit ring map
kk = ZZ/101
R = kk[a]
M = first pushFwd id_R
assert(M == module R)
-- repeat pushforward of ring map works
M' = first pushFwd id_R
assert(M == module R)
///

-- test 32
TEST ///
-- from examples
kk = ZZ/101
A = kk[x]
B = A[y,z,Join => false]/(y^3 - x*z, z^3-y^7)
fy = pushFwd matrix y
fz = pushFwd matrix z
fx = pushFwd matrix x_B
g = pushFwd matrix(y*z -x_B*z^2)

assert(g == fy*fz-fx*fz^2)
assert(fz^3-fy^7 == 0)
///

-- test 33
TEST ///
-- pushforward of skew ring along map which is not the inclusion of its coefficient ring
kk = ZZ/101
R = kk[a..c, SkewCommutative => {b, c}]
T = kk[t]
f = map(R, T, {a^2})
(pR, matR, pf) = pushFwd f
assert(degree(pR) == 8)

I = ideal vars R
M = I / I^2
g = map(R, T, {a^3})
pM = pushFwd(g, M)
pms = pM_{0..numgens pM - 1}
assert(pms == pushforward(g, pushforward' pms))
ms = M_{0..numgens M - 1}
assert(ms ==  pushforward' pushforward(g, ms))
///

-- test 34
TEST ///
-- this test is a somewhat technical guardrail showing the necessity of
-- untwisting by the antipode map when we apply pushforward to elements.
kk = ZZ/101
R = kk[a..e, SkewCommutative => true]
f = map(R, kk)
M = R^2
gensM = M_{0..numgens M - 1}
M' = pushFwd(f, M)
gensM' = M'_{0..numgens M' - 1}

assert(gensM' == pushforward(f, pushforward' gensM'))
assert(gensM == pushforward' pushforward(f, gensM))
///

-- test 35
TEST ///
-- the case of the zero module shows up naturally when applying pushFwd to a
-- free resolution which is why we are fussing over it.
kk = ZZ/3
R = kk[a]/a^2
f = map(R, kk)
M = R^0
N = pushFwd(f, M)
assert(pushforward' pushforward(N, 0_M) == 0_M)
///

--- Hom tests ---

TEST ///
-- commutative case: pushFwd(f, Hom(-, 0))Hom and Hom(f, -, -) agree on the nose
kk = ZZ/101
R = kk[x, y]
S = kk[s, t]
f = map(R, S, {x^2, y^3})

I = ideal vars R
M = R^1/I
N = I/I^2
fH = pushFwd(f, Hom(M, N))
H = Hom(f, M, N)
assert(H == fH)

-- homomorphism and homomorphism' act like expected
-- the degrees are messed up
r = apply(0..numgens H - 1, i -> (homomorphism'(f, homomorphism matrix H_i), matrix H_i))
scan(0..numgens H - 1, i -> assert(homomorphism'(f, homomorphism H_{i}) - H_{i} == 0))
///

TEST ///
-- commutative case: pushFwd(f, Hom(-, -))Hom and Hom(f, -, -) are isomorphic not equal
kk = ZZ/101
R = kk[x, y]
S = kk[s, t]
f = map(R, S, {x^3 + x * y + y, y^2 + 2*x})

M = module ideal {x + y}
N = module ideal {x^2, x*y, y + 1}
fH = pushFwd(f, Hom(M, N))
H = Hom(f, M, N)
-- these modules are presented differently
assert(fH != H)

phi = map(fH, H, matrix {for i from 0 to numgens H - 1 list pushforward(fH, homomorphism' homomorphism H_{i})})
phi' = map(H, fH, matrix {for i from 0 to numgens fH - 1 list homomorphism'(f, homomorphism pushforward' fH_{i})})
assert(phi * phi' == id_fH)
assert(phi' * phi == id_H)

-- homomorphism and homomorphism' act like expected
-- the degrees are messed up
scan(0..numgens H - 1, i -> assert(homomorphism'(f, homomorphism H_{i}) - H_{i} == 0))
///

TEST ///
-- skew symmetric homs
kk = ZZ/101
R = kk[a..c, SkewCommutative => true]
f = map(R, kk)
M = module ideal {a + b}
N = module ideal {a*b, b + 1}
H = Hom(f, M, N)
mapped = apply(0..numgens H - 1, i -> (homomorphism H_{i}) M_0)
scan(mapped, y -> assert(module y == N))

-- homomorphism and homomorphism' act like expected
scan(0..numgens H - 1, i -> assert(homomorphism'(f, homomorphism H_{i}) == H_{i}))
///

TEST ///
-- skew symmetric endomorphisms
kk = ZZ/101
R = kk[a..c, SkewCommutative => true]
f = map(R, kk)
M = module ideal {a + b}
H = End(f, M)
-- we have the identity morphism
assert(homomorphism homomorphism'(f, id_M) == id_M)
-- homomorphism and homomorphism' act like expected
scan(0..numgens H - 1, i -> assert(homomorphism'(f, homomorphism H_{i}) == H_{i}))
///

TEST ///
-- skew symmetric computation
-- compare Hom(f, - , F) with Hom(-, F) when F is free since we can compute this directly over R in that case
kk = ZZ/11
R = kk[a..c, SkewCommutative => true]
f = map(R, kk)
H = Hom(f, R^2, R^1)
H' = Hom(R^2, R^1) -- R^1 is a bimodule so Hom can actually be computed as an R-module
fH' = pushFwd(f, H')

phi = map(H, fH', matrix {for i from 0 to numgens fH' - 1 list homomorphism'(f, homomorphism pushforward' fH'_{i})})
assert(phi * phi^-1 == id_H)
assert(phi^-1 * phi == id_fH')
///

TEST ///
-- Compute Ext over commutative ring finite map
-- compare pushFwd(f, Ext^i(M, N)) with Ext^i(f, M, N)
kk = ZZ/11
R = kk[s, t]/ ideal {s^2, t^3}
f = map(R, kk)
I = ideal vars R
M = R^1/I
N = I / I^3

-- Ext^1
E = Ext^1(f, M, N)
E' = pushFwd(f, Ext^1(M, N))

imgs = matrix {for i from 0 to numgens E - 1 list pushforward(E', yonedaExtension' yonedaExtension E_i)}
phi = map(E', E, imgs)
assert(phi * phi^-1 == id_E')
assert(phi^-1 * phi == id_E)

-- Ext^2
E = Ext^2(f, M, N)
E' = pushFwd(f, Ext^2(M, N))

imgs = matrix {for i from 0 to numgens E - 1 list pushforward(E', yonedaExtension' yonedaExtension E_i)}
phi = map(E', E, imgs)
assert(phi * phi^-1 == id_E')
assert(phi^-1 * phi == id_E)
///

TEST ///
-- Compute Ext over commutative ring non-finite map but finite modules
-- compare pushFwd(f, Ext^i(M, N)) with Ext^i(f, M, N)
kk = ZZ/11
R = kk[s, t]/ ideal {t^3}
S = kk[a]
f = map(R, S, {s^2})
I = ideal vars R
M = R^1/I
N = I / I^3

-- Ext^1
E = Ext^1(f, M, N)
E' = pushFwd(f, Ext^1(M, N))

imgs = matrix {for i from 0 to numgens E - 1 list pushforward(E', yonedaExtension' yonedaExtension E_i)}
phi = map(E', E, imgs)
assert(phi * phi^-1 == id_E')
assert(phi^-1 * phi == id_E)

-- Ext^2
E = Ext^2(f, M, N)
E' = pushFwd(f, Ext^2(M, N))

imgs = matrix {for i from 0 to numgens E - 1 list pushforward(E', yonedaExtension' yonedaExtension E_i)}
phi = map(E', E, imgs)
assert(phi * phi^-1 == id_E')
assert(phi^-1 * phi == id_E)
///

TEST ///
-- Compute Ext over skew-symmetric algebra
-- check that yonedaExtension and yonedaExtension' are inverses
kk = ZZ/11
R = kk[a..c, SkewCommutative => true]
f = map(R, kk)
M = ideal {a*b, b + c}
N = ideal {b*c, a * c}

-- Ext^1
E = Ext^1(f, M, N)
imgs = matrix {for i from 0 to numgens E - 1 list yonedaExtension'(f, yonedaExtension E_i)}
assert(map(E, E, imgs) == id_E)

-- Ext^2
E = Ext^2(f, M, N)
imgs = matrix {for i from 0 to numgens E - 1 list yonedaExtension'(f, yonedaExtension E_i)}
assert(map(E, E, imgs) == id_E)
///

-- the following test cases are all "bugs" that did not run successfully but
-- have now been fixed.
TEST ///
kk = ZZ/101
A = kk[s,t]
C = A[x,y,z]/(x^2, y^2, z^2)
phi = map(C,A)
f = map(C^1, A^4, phi, {{x,s*y,t*y, z}})
ker f
///

TEST ///
kk = ZZ/101
A = kk[s,t]
B = frac A
C = B[x,y,z]/(x^2, y^2, z^2)
phi = map(C,B)
f = map(C^1, B^3, phi, {{x,s*y,z}})
ker f
///

TEST ///
s = symbol s; t = symbol t
kk = ZZ/101
A = frac(kk[s,t])
L = A[symbol a.. symbol d]/(d-t, a-s, b*c-s*t, b^2-(s/t)*c^2)
describe L
ML = pushFwd(map(L,frac A), L^1) -- dim 4
///

TEST ///
debug needsPackage "PushForward"
s = symbol s; t = symbol t
kk = ZZ/101
A = frac(kk[s,t])
L = A[symbol b, symbol c]/(b*c-s*t, b^2-(s/t)*c^2)
basis L
describe L
inc = map(L, A)
assert isInclusionOfCoefficientRing inc
assert isModuleFinite L
pushFwd inc
ML = pushFwd(map(L,frac A), L^1)
///

TEST ///
debug needsPackage "PushForward"
s = symbol s; t = symbol t
A = QQ
L = A[symbol b, symbol c]/(b*c-13, b^3-c^2)
describe L
inc = map(L, A)
assert isInclusionOfCoefficientRing inc
assert isModuleFinite L
(LA, bas, pf) = pushFwd inc -- this works
pf(b^2+c^2)
///

TEST ///
  s = symbol s; t = symbol t
  kk = ZZ/101
  A = frac(kk[s,t])
  L = A[symbol b, symbol c]/(b^2-(s/t)*c^2 - c, c^3)
  basis L
  describe L
  inc = map(L, A)
  pushFwd inc
///