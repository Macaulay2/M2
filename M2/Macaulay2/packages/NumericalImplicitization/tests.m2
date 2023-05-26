
TEST /// -- embedding cubic surface (with 3 singular points) in P^3 via 5 sections of O(2)
setRandomSeed 0
elapsedTime d = dim ker map(QQ[x,y,z,w]/ideal(x^3 - y*z*w), QQ[a_0..a_4], {x*w + 2*x*y, x*w-3*y^2, z^2, x^2 + y^2 + z^2 - w^2, 3*y*w - 2*x^2})
R = CC[x,y,z,w]
I = ideal(x^3 - y*z*w)
F = {x*w + 2*x*y, x*w-3*y^2, z^2, x^2 + y^2 + z^2 - w^2, 3*y*w - 2*x^2}
assert(numericalImageDim(F, I) == d)
-- Cf. also: non-homogeneous ideal (x^5 - y*z*w) (~35 seconds for GB computation), kernel over finite fields
///

TEST /// -- twisted cubic
setRandomSeed 0
R = CC[s,t]
F = basis(3,R)
J = monomialCurveIdeal(QQ[a_0..a_3], {1,2,3})
assert(all(1..5, d -> (numericalHilbertFunction(F,ideal 0_R,d)).hilbertFunctionValue == numcols super basis(d,J)))
assert(all(1..5, d -> (numericalHilbertFunction(F,ideal 0_R,d,UseSLP=>true)).hilbertFunctionValue == numcols super basis(d,J)))
W = pseudoWitnessSet(F, ideal 0_R);
assert(W.degree == 3)
assert(isOnImage(W, first numericalImageSample(F,ideal 0_R)) == true)
assert(isOnImage(W, point random(CC^1,CC^(numcols F))) == false)
///

TEST /// -- Rational quartic curve in P^3
setRandomSeed 0
R = CC[s,t]
F = flatten entries basis(4, R) - set{s^2*t^2}
I = ideal 0_R
S = QQ[a_0..a_3]
I3 = super basis(3, ker map(QQ[s,t], S, {s^4,s^3*t,s*t^3,t^4}))
T = numericalHilbertFunction(F, I, 3);
M = extractImageEquations(T, AttemptZZ => true)
assert(image M == image (map(ring M, S, gens ring M))(I3))
elapsedTime PW = pseudoWitnessSet(F,I)
assert(PW.degree == 4)
///

TEST /// -- Grassmannian Gr(3, 5) = G(P^2,P^4)
setRandomSeed 0
(k, n) = (3,5)
R = CC[x_(1,1)..x_(k,n)]
I = ideal 0_R
F = (minors(k, genericMatrix(R, k, n)))_*
assert(numericalImageDim(F, I) == 1 + k*(n-k))
T = numericalHilbertFunction(F, I, 2)
J = super basis(2, Grassmannian(k-1,n-1))
assert(T.hilbertFunctionValue == numcols J)
I2 = image extractImageEquations(T, AttemptZZ => true)
assert(image (map(ring I2, ring J, gens ring I2))(J) == I2)
time W = pseudoWitnessSet(F, I, Repeats => 2)
assert(W.degree == 5)
-- (n, m) = (5, 10)
-- pointList = numericalImageSample(F, I, n);
-- assert(all(pointList, q -> (tally apply(m, i -> isOnImage(W, q)))#true / m >= 8/10)) -- too slow for test
///

-* disabled due to issue 2230
TEST /// -- random canonical curve of genus 4, under random projection to P^2 by cubics
setRandomSeed 0
R = CC[x_0..x_3]
I = ideal(random(2,R),random(3,R))
F = random(R^1,R^{3:-3})
assert(numericalImageDegree(F,I) == 18)
assert((numericalHilbertFunction(F,I,18)).hilbertFunctionValue == 1)
///
*-

TEST /// -- Segre + Veronese
setRandomSeed 0
-- Veronese surface P^2 in P^5
(d, n) = (2, 2)
R = CC[x_0..x_n]
F = basis(d, R)
PW = pseudoWitnessSet(F, ideal 0_R)
assert(PW.degree == 4)
assert((pseudoWitnessSet(PW.map, PW.sourceEquations, PW.witnessPointPairs_{1}, PW.imageSlice)).degree == 4)
assert((pseudoWitnessSet(PW.map, PW.sourceEquations, PW.witnessPointPairs_{0,2}, PW.imageSlice)).degree == 4)
I2 = ideal extractImageEquations(F, ideal 0_R, 2, AttemptZZ => true)
S = QQ[y_0..y_(binomial(d+n,d)-1)]
RQ = QQ[x_0..x_n]
J = ker map(RQ, S, basis(d, RQ))
assert((map(ring I2, S, gens ring I2))(J) == I2)
-- Segre P^2 x P^3
(n1, n2) = (2, 4)
R = CC[s_0..s_(n1), t_0..t_(n2)]
F = (ideal(s_0..s_(n1))*ideal(t_0..t_(n2)))_*
I2 = ideal extractImageEquations(F, ideal 0_R, 2, AttemptZZ => true)
RQ = QQ[s_0..s_(n1), t_0..t_(n2)]
S = QQ[y_0..y_((n1+1)*(n2+1)-1)]
J = ker map(RQ, S, (ideal(s_0..s_(n1))*ideal(t_0..t_(n2)))_*)
assert((map(ring I2, S, gens ring I2))(J) == I2)
///

TEST /// -- Iterated Veronese
(d1,d2) = (2,3)
R = CC[x_0..x_(d1)]
I = ideal(x_0*x_2 - x_1^2)
W = first components numericalIrreducibleDecomposition I
I.cache.WitnessSet = W;
S = CC[y_0..y_(binomial(d1+d2,d2)-1)]
F = map(R,S,basis(d2,R))
eps = 1e-10
p1 = first numericalSourceSample(I)
assert(clean(eps, sub(gens I, matrix p1)) == 0)
p2 = first numericalSourceSample(I, W)
assert(clean(eps, sub(gens I, matrix p2)) == 0)
p3 = first numericalSourceSample(I, p1)
assert(clean(eps, sub(gens I, matrix p3)) == 0)
P = numericalSourceSample(I, W, 10)
assert(all(P/matrix, p -> clean(eps, sub(gens I, p)) == 0))
q1 = numericalImageSample(F, I)
S = numericalImageSample(F,I,55);
T = numericalHilbertFunction(F,I,S,2)
assert(T.hilbertFunctionValue == 42)
assert(isWellDefined T)
PW = pseudoWitnessSet(F, I, p1)
assert(isWellDefined PW)
assert(PW.degree == 6)
assert(numericalImageDegree(F, I) == 6)
assert((pseudoWitnessSet(F, I, {PW.witnessPointPairs#0}, PW.imageSlice)).degree == 6)
assert((pseudoWitnessSet(F, I, PW.witnessPointPairs, PW.imageSlice)).degree == 6)
///

TEST /// -- Orthogonal group O(n)
setRandomSeed 0
n = 4
R = CC[x_0..x_(n^2-1)]
A = genericMatrix(R,n,n)
I = ideal(A*transpose A - id_(R^n));
F = vars R
p = point id_((coefficientRing R)^n)
assert(numericalImageDim(F,I,p) == binomial(n,2))
degSOn = 2^(n-1)*det matrix table(floor(n/2), floor(n/2), (i,j) -> binomial(2*n - 2*i - 2*j - 4, n - 2*i - 2))
elapsedTime PW = pseudoWitnessSet(F,I,p, Repeats=>2, Threshold=>3, MaxThreads=>allowableThreads) -- ~ 40 seconds
assert(numericalImageDegree PW == degSOn)
///

TEST /// -- Twisted cubic projections
R = CC[x_0..x_3]
I = monomialCurveIdeal(R, {1,2,3})
F1 = random(R^1, R^{3:-1})
p = numericalSourceSample I
imagePts = numericalImageSample(F1, I, 10);
assert(numericalImageDim(F1, I, p#0) == 2)
assert((numericalHilbertFunction(F1, I, imagePts, 2)).hilbertFunctionValue == 0)
assert((numericalHilbertFunction(F1, I, imagePts, 3)).hilbertFunctionValue == 1)
F2 = (gens R)_{0,2,3}
T = numericalHilbertFunction(F2, I, 3)
nodalCubic = ideal extractImageEquations(T, AttemptZZ => true)
S = ring nodalCubic
assert(nodalCubic == ideal(S_1^3 - S_0*S_2^2))
F3 = (gens R)_{0,1,2}
assert((numericalHilbertFunction(F3, I, 2)).hilbertFunctionValue == 1)
assert((pseudoWitnessSet(F2, I, p#0)).degree == 3)
///

TEST /// -- 3x3 matrices with double eigenvalue
S = QQ[a_(0,0)..a_(2,2), lambda, mu]
B = transpose genericMatrix(S,3,3)
J = ideal(B*transpose(B) - id_(S^3), det(B)-1)
n = B*diagonalMatrix{lambda,lambda,mu}*transpose(B)
T = QQ[p_(0,0),p_(0,1),p_(0,2),p_(1,1),p_(1,2),p_(2,2)]
J = ker map(S/J,T,{n_(0,0),n_(0,1),n_(0,2),n_(1,1),n_(1,2),n_(2,2)})
R = CC[a_(0,0)..a_(2,2), lambda, mu]
A = transpose genericMatrix(R,3,3)
I = ideal(A*transpose(A) - id_(R^3), det(A)-1)
m = A*diagonalMatrix{lambda,lambda,mu}*transpose(A)
F = {m_(0,0),m_(0,1),m_(0,2),m_(1,1),m_(1,2),m_(2,2)}
p = point{flatten entries diagonalMatrix {-1,-1,1} | {-1_CC, 1}}
assert(numericalImageDim(F,I,p) == dim J)
assert((pseudoWitnessSet(F,I,p)).degree == degree J)
///

TEST /// -- Numerical nullity tests
assert(numericalNullity(matrix{{2, 1}, {0, 1e-7}}, Precondition => false) == 1)
assert(numericalNullity(map(CC^2,CC^2,0)) == 2)
assert(numericalNullity(id_(CC^2)) == 0)
assert(numericalNullity(random(CC^2,CC^2)) == 0)
assert(numericalNullity(random(CC^0,CC^2)) == 2)
assert(numericalNullity(random(CC^2,CC^0)) == 0)
assert(numericalNullity(random(CC^0,CC^0)) == 0)
///

TEST /// -- approxPoint tests
(n,r) = (4,5)
R = QQ[x_(1,1)..x_(n,r)]
A = transpose genericMatrix(R,r,n)
I1 = ideal(A*transpose A - (r/n)*id_(R^n))
I2 = ideal apply(entries transpose A, row -> sum(row, v -> v^2) - 1)
I = I1 + I2 -- funtf variety
-- setRandomSeed 5
elapsedTime q = first numericalSourceSample(I, Software => I -> realPoint(I, Iterations => 100))
assert(norm evaluate(gens I, q) < 1e-5)
///
