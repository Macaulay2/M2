TEST ///
R = QQ[x,y,z]/(ideal(x^3,y^3,z^3,x*y*z))
A = acyclicClosure(R,EndDegree => 2)
X = blockDiff(A,3)
Y = blockDiff(A,2)
--reconstruct a composite block:
assert(0 == Y_[{2,0,0}]^[{1,0,0}] *X^[{2,0,0}]_[{1,2,0}] + Y_[{0,2,0}]^[{1,0,0}] *X^[{0,2,0}]_[{1,2,0}] )

///
TEST ///
-- test 0 : isHomogeneous, toComplex, maxDegree
R = ZZ/101[x,y,z]
A1 = freeDGAlgebra(R,{{1},{1},{1},{3}})
setDiff(A1,{x,y,z,x*T_(1,2)*T_(1,3)-y*T_(1,1)*T_(1,3)+z*T_(1,1)*T_(1,2)})
assert(not A1.isHomogeneous)
A1dd = toComplex(A1)
A1dd.dd

A2 = freeDGAlgebra(R,{{1,1},{1,1},{1,1},{3,3}})
setDiff(A2,{x,y,z,x*T_(1,2)*T_(1,3)-y*T_(1,1)*T_(1,3)+z*T_(1,1)*T_(1,2)})
assert(A2.isHomogeneous)
A2dd = toComplex(A2)
A2dd.dd

B1 = koszulComplexDGA(R)
assert(B1.isHomogeneous)
B1dd = toComplex(B1)
B1dd.dd

R = ZZ/101[x,y,z]
R2 = R/ideal {x^2-z^3}
B2 = koszulComplexDGA(R2)
assert(not B2.isHomogeneous)
B2dd = toComplex(B2)
B2dd.dd

R = QQ[x,y,z]
B = koszulComplexDGA(R)
toComplex(B)
degrees B.natural
A = freeDGAlgebra(R,{{1},{1},{1},{3}})
setDiff(A,{x,y,z,x*T_(1,2)*T_(1,3)-y*T_(1,1)*T_(1,3)+z*T_(1,1)*T_(1,2)})
Add = toComplex(A)
assert(apply(maxDegree(A)+1, i -> prune HH_i(Add)) == {coker vars R,0,0,coker vars R,0,0,0})
///

TEST ///
-- test 1 : differential tests
R = ZZ/101[x,y,z, Degrees => {2,2,3}]
kRes = freeResolution coker vars R
kRes.dd_3
A = koszulComplexDGA(R)
d3 = A.dd_3
d2 = A.dd_2
d1 = A.dd_1
assert(source d1 == target d2)
assert(source d2 == target d3)
assert(d1*d2 == 0)
assert(d2*d3 == 0)
S1 = R/ideal (x^3-z^2)
B1 = koszulComplexDGA(S1)
d3 = B1.dd_3
d2 = B1.dd_2
d1 = B1.dd_1
assert(source d1 == target d2)
assert(source d2 == target d3)
assert(d1*d2 == 0)
assert(d2*d3 == 0)
use R
S2 = R/ideal (x^4-z^2)
B2 = koszulComplexDGA(S2)
d3 = B2.dd_3
d2 = B2.dd_2
d1 = B2.dd_1
assert(source d1 == target d2)
assert(source d2 == target d3)
assert(d1*d2 == 0)
assert(d2*d3 == 0)
///

TEST ///
--- test 2 : homology, homologyAlgebra, HH_ZZ, HH
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
HH_2(A)
HH(A)
hh2 = prune HH_2(koszulR)
hh2' = prune HH_2(A)
assert(hh2 == hh2')
///

TEST ///
-- test 3 : torAlgebra, deviations
R1 = QQ[x,y,z]/ideal{x^3,y^4,z^5}
TorR1 = torAlgebra(R1,GenDegreeLimit=>4)
devR1 = deviations(R1,DegreeLimit=>4)
use R1
M = coker matrix {{x^2*y^3*z^4}}
Mres = freeResolution(M, LengthLimit => 7)
R2 = QQ[x,y,z]/ideal{x^3,y^4,z^5,x^2*y^3*z^4}
time TorR1R2 = torAlgebra(R1,R2,GenDegreeLimit=>5,RelDegreeLimit=>10)
-- the multiplication is trivial, since the map R3 --> R4 is Golod
numgens TorR1R2
numgens ideal TorR1R2
apply(21, i -> #(flatten entries getBasis(i,TorR1R2)))
assert(sum oo - 1 == numgens TorR1R2)
///

TEST ///
-- test 4 : findEasyRelations
debug DGAlgebras
R1 = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
R2 = ZZ/32003[a,b,x,y,Degrees=>{1,1,2,2}]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
A1 = koszulComplexDGA(R1)
A2 = koszulComplexDGA(R2)
cycleList1 = getGenerators(A1,DegreeLimit=>4)
cycleList2 = getGenerators(A2,DegreeLimit=>4)
HAEasy1 = findEasyRelations(A1,cycleList1)
HAEasy2 = findEasyRelations(A2,cycleList2)
tally ((flatten entries basis HAEasy1) / degree)
pairs (tally ((flatten entries basis HAEasy1) / degree))
myList1 = {({4,8},1),({3,4},1),({3,5},6),({3,6},6),({3,7},4),({2,3},4),({2,4},11),({2,5},8),({2,6},4),({1,2},4),({1,3},4),({1,4},1),({0,0},1)}
myList2 = {({0},1),({1},9),({2},27),({3},17),({4},1)}
tally ((flatten entries basis HAEasy1) / degree)
tally myList1
assert(pairs tally((flatten entries basis HAEasy1) / degree) == myList1)
assert(pairs tally((flatten entries basis HAEasy2) / degree) == myList2)
///

TEST ///
-- test 5 : homology of a DGA whose H_0 is not a field
R = ZZ/32003[a,b]
I = ideal{a^6,b^6}
A = koszulComplexDGA(I)
HA = HH A
describe HA
use R
J = I + ideal {a^4*b^5,a^5*b^4}
B = koszulComplexDGA(J)
getGenerators(B)
apply(5, i -> numgens prune homology(i,B))
apply(5, i -> prune homology(i,B))
HB = HH B
HB2 = zerothHomology B
HB.cache.cycles
ideal HB
-- looks right...
getDegNModule(0,HB2,HB)
getDegNModule(1,HB2,HB)
getDegNModule(2,HB2,HB)
getDegNModule(3,HB2,HB)
getDegNModule(4,HB2,HB)

R = ZZ/32003[a,b,c]
I = (ideal vars R)^2
A = koszulComplexDGA(I)
apply(10, i -> prune homology(i,A))
time HA = HH A
HA2 = zerothHomology A
tally ((ideal HA)_* / degree / first)
select ((ideal HA)_*, f -> first degree f == 2)
-- looks right...
getDegNModule(0,HA2,HA)
getDegNModule(1,HA2,HA)
getDegNModule(2,HA2,HA)
getDegNModule(3,HA2,HA)
-- need to add asserts
///

TEST ///
-- test 6 : homologyAlgebra
R = ZZ/32003[a,b,x,y]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR = koszul vars R
time apply(5,i -> numgens prune HH_i(koszulR))
A = koszulComplexDGA(R)
time apply(5,i -> numgens prune homology(i,A))
-- ~2.15 seconds on mbp, with graded differentials
time HA = HH A
assert(numgens HA == 34)
assert(numgens ideal HA == 576)
assert(#(first degrees HA) == 2)

-- same example, but not graded because of the degree change.  The homologyAlgebra function
-- will then only return a graded algebra
R2 = ZZ/32003[a,b,x,y,Degrees=>{1,1,2,2}]/ideal{a^3,b^3,x^3,y^3,a*x,a*y,b*x,b*y,a^2*b^2-x^2*y^2}
koszulR2 = koszul vars R2
time apply(5,i -> numgens prune HH_i(koszulR2))
A2 = koszulComplexDGA(R2)
time apply(5,i -> numgens prune homology(i,A2))
-- ~2.85 seconds on mbp, with ungraded differentials
time HA2 = homologyAlgebra A2
assert(numgens HA2 == 34)
assert(numgens ideal HA2 == 576)
-- should only be singly graded
assert(#(first degrees HA2) == 1)
///

TEST ///
-- test 7 : acyclicClosure, isHomologyAlgebraTrivial, isGolod, isGolodHomomorphism
R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
M = coker matrix {{a^3*b^3*c^3*d^3}};
S = R/ideal{a^3*b^3*c^3*d^3}
time A = acyclicClosure(R,EndDegree=>6)
B = A ** S
assert(isHomologyAlgebraTrivial(B,GenDegreeLimit=>6))
assert(isGolodHomomorphism(S,GenDegreeLimit=>6,TMOLimit=>3))
-- returns true since R --> S is Golod
R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
A = koszulComplexDGA(R)
assert(not isHomologyAlgebraTrivial(A))
assert(not isGolod R)
-- false, since R is Gorenstein, and so HA has Poincare Duality
///

TEST ///
-- test 8 : DGAlgebra ** DGAlgebra - need to add in an assert
R = ZZ/101[a,b,c,d]
I = ideal(a,b)
J = ideal(c,d)
A = koszulComplexDGA(I)
B = koszulComplexDGA(J)
Cdd = toComplex(A ** B)
Cdd.dd
///

TEST ///
-- test 9 : isHomologyAlgebraTrivial, getGenerators, findTrivialMasseyOperation
Q = ZZ/101[x_1..x_6]
I = ideal (x_3*x_5,x_4*x_5,x_1*x_6,x_3*x_6,x_4*x_6)
R = Q/I
A = koszulComplexDGA(R)
isHomologyAlgebraTrivial(A,GenDegreeLimit=>3)
cycleList = getGenerators(A)
assert(first findTrivialMasseyOperation(A))

-- this is a Teter ring, and the computation in Avramov and Levin's paper shows
-- H(A) does not have trivial multiplication.
Q = ZZ/101[x,y,z]
I = ideal (x^3,y^3,z^3,x^2*y^2*z^2)
R = Q/I
A = koszulComplexDGA(R)
assert(not isHomologyAlgebraTrivial(A,GenDegreeLimit=>3))
cycleList = getGenerators(A)
prodList = apply(subsets(cycleList,2), l -> (first degree l#0 + first degree l#1,l#0*l#1));
assert(not first findTrivialMasseyOperation(A))
///

TEST ///
-- test 10 : isAcyclic
R = ZZ/101[a,b,c,d]
A = koszulComplexDGA(R)
B = koszulComplexDGA({a^4,b^4,c^4,d^4})
C = koszulComplexDGA((ideal vars R)^2)
assert(isAcyclic A)
assert(isAcyclic B)
assert(not isAcyclic C)
///

TEST ///
-- test 11 : isGolod and isHomologyAlgebraTrivial example
-- Interesting case due to Katthan.
Q = ZZ/101[x_1,x_2,y_1,y_2,z,w]
I = ideal {x_1*x_2^2,z^2*w,y_1*y_2^2,x_2^2*z*w,y_2^2*z^2,x_1*x_2*y_1*y_2,x_2^2*y_2^2*z,x_1*y_1*z}
R = Q/I
assert(isHomologyAlgebraTrivial koszulComplexDGA R == true)
assert(isGolod R == false)
///

TEST ///
--- test 12: isGolod and isHomologyAlgebraTrivial example again
--- This example is due to Roos
S = QQ[x,y,z,u]
I = ideal(u^3, x*y^2, (x+y)*z^2, x^2*u+z*u^2, y^2*u+x*z*u, y^2*z+y*z^2)
 -- you can see that the mult on the koszul homology will be trivial
betti (A = freeResolution I)
R = S/I
assert(isHomologyAlgebraTrivial koszulComplexDGA R == true)
assert(isGolod R == false)
///

TEST ///
-- test 13: freeDGAlgebra variable-naming convention and Variable option
R = QQ[x, y, z]
A = freeDGAlgebra(R, {{1}, {1}, {1}, {3}})
-- Default base symbol T, doubly-indexed by (hom-degree, counter).
assert(toString gens A.natural == "{T_(1,1), T_(1,2), T_(1,3), T_(3,1)}")
B = freeDGAlgebra(R, {{1}, {1}, {1}, {3}}, Variable => "U")
assert(toString gens B.natural == "{U_(1,1), U_(1,2), U_(1,3), U_(3,1)}")
-- List form: each symbol supplied verbatim.
C = freeDGAlgebra(R, {{1}, {1}, {1}}, Variable => {getSymbol "P", getSymbol "Q", getSymbol "RR"})
assert(toString gens C.natural == "{P, Q, RR}")
assert(numgens C.natural == 3)
-- Multigrade vs single-grade homogeneity.
D1 = freeDGAlgebra(R, {{1, 1}, {1, 1}, {1, 1}, {3, 3}})
setDiff(D1, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)})
assert(isHomogeneous D1)
D2 = freeDGAlgebra(R, {{1}, {1}, {1}, {3}})
setDiff(D2, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)})
assert(not isHomogeneous D2)
///

TEST ///
-- test 14: koszulComplexDGA naming convention and Variable option
R = QQ[a, b, c]
A = koszulComplexDGA R
assert(toString gens A.natural == "{T_(1,1), T_(1,2), T_(1,3)}")
AS = koszulComplexDGA(R, Variable => "S")
assert(toString gens AS.natural == "{S_(1,1), S_(1,2), S_(1,3)}")
-- Koszul complex on a regular sequence has acyclic higher homology.
assert(isAcyclic A)
-- Differentials on Koszul gens match the ring vars.
natGens = gens A.natural
assert((A.diff) natGens_0 == sub(a, A.natural))
assert((A.diff) natGens_1 == sub(b, A.natural))
assert((A.diff) natGens_2 == sub(c, A.natural))
-- (koszulComplexDGA, List) form: regular sequence in polynomial ring.
S = QQ[x, y, z]
B = koszulComplexDGA({x^2, y*z})
assert(numgens B.natural == 2)
-- (x^2, yz) is regular in QQ[x,y,z], so Koszul homology vanishes in deg > 0.
assert(prune HH_1 toComplex B == 0)
assert(prune HH_2 toComplex B == 0)
///

TEST ///
-- test 15: acyclicClosure Tate construction and variable convention
-- C.i. case: exactly three hom-degree-2 gens kill the three Koszul relation cycles.
R = ZZ/101[a, b, c] / ideal(a^3, b^3, c^3)
A = koszulComplexDGA R
B = acyclicClosure(A, EndDegree => 2)
assert(numgens B.natural == 6)
-- The Tate theorem: for a c.i., no further gens appear past hom-degree 2.
B2 = acyclicClosure(A, EndDegree => 4)
assert(numgens B2.natural == numgens B.natural)
-- Differentials on the newly adjoined generators are a_i^2 * T_(1,i).
(aa, bb, cc) = (sub(a, B.natural), sub(b, B.natural), sub(c, B.natural))
natGens = gens B.natural
-- First three gens are T_(1,*) with diff a,b,c.
assert((B.diff) natGens_0 == aa)
assert((B.diff) natGens_1 == bb)
assert((B.diff) natGens_2 == cc)
-- Next three gens are T_(2,*) with diff a_i^2 * T_(1,i).
assert((B.diff) natGens_3 == aa^2 * natGens_0)
assert((B.diff) natGens_4 == bb^2 * natGens_1)
assert((B.diff) natGens_5 == cc^2 * natGens_2)
-- Variable => ... renames only the new generators.
D = acyclicClosure(A, EndDegree => 2, Variable => "U")
assert(toString take(gens D.natural, 3) == "{T_(1,1), T_(1,2), T_(1,3)}")
assert(toString drop(gens D.natural, 3) == "{U_(2,1), U_(2,2), U_(2,3)}")
///

TEST ///
-- test 16: acyclicClosure deviation sequences
-- Non-c.i. has higher deviations; c.i. does not.
S = QQ[x, y, z] / ideal(x^2, y^2, z^2)
CS = acyclicClosure(S, EndDegree => 4)
homdegCounts = tally apply(CS.Degrees, d -> first d)
assert(homdegCounts#1 == 3)     -- three Koszul exterior gens
assert(homdegCounts#2 == 3)     -- three Tate polynomial gens
assert(not homdegCounts#?3)     -- no hom-deg 3 gens (c.i.)
assert(not homdegCounts#?4)     -- no hom-deg 4 gens (c.i.)
T = QQ[x, y] / ideal(x^2, x*y, y^2)
CT = acyclicClosure(T, EndDegree => 3)
homdegCountsT = tally apply(CT.Degrees, d -> first d)
assert(homdegCountsT#1 == 2)    -- two Koszul exterior gens
assert(homdegCountsT#2 == 3)    -- three relation-killers
assert(homdegCountsT#?3)        -- non-c.i. has hom-deg 3 deviation
///

TEST ///
-- test 17: adjoinVariables preserves existing names, cycleCheck via polyDifferential
R = ZZ/101[a, b, c] / ideal(a^3, b^3, c^3)
A = koszulComplexDGA R
z = a^2 * (gens A.natural)_0
assert(polyDifferential(A, z) == 0)
B = adjoinVariables(A, {z})
assert(numgens B.natural == numgens A.natural + 1)
-- First three gens of B preserve A's names; new gen continues the counter.
assert(toString take(gens B.natural, 3) == "{T_(1,1), T_(1,2), T_(1,3)}")
-- Variable => ... renames only the new generator.
C = adjoinVariables(A, {z}, Variable => "U")
assert(toString last gens C.natural == "U_(2,1)")
-- The added generator's differential is exactly the cycle z.
assert((B.diff) last gens B.natural == sub(z, B.natural))
///

TEST ///
-- test 18: setDiff ordering, validity via isWellDefined
R = ZZ/101[x, y, z]
A = freeDGAlgebra(R, {{1}, {1}, {1}, {3}})
setDiff(A, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)})
-- Differentials are in gen-order matching A.Degrees.
diffs = take(flatten entries matrix A.diff, numgens A.natural)
assert(diffs#0 == x)
assert(diffs#1 == y)
assert(diffs#2 == z)
assert(isWellDefined A)
///

TEST ///
-- test 19: freeDGModule construction, setDiff, and matrix factorization
-- 2-periodic min. free resolution of k = R/x over R = QQ[x]/(x^2).
R = QQ[x] / ideal(x^2)
A = koszulComplexDGA R
F = freeDGModule(A, toList(0..4))
natGens = apply(rank F.natural, i -> (F.natural)_i)
setDiff(F, {0, x * natGens#0, x * natGens#1, x * natGens#2, x * natGens#3})
-- Every consecutive pair of differentials composes to zero.
d1 = moduleDifferential(1, F)
d2 = moduleDifferential(2, F)
d3 = moduleDifferential(3, F)
d4 = moduleDifferential(4, F)
assert(d1 * d2 == 0)
assert(d2 * d3 == 0)
assert(d3 * d4 == 0)
assert(isWellDefined F)
///

TEST ///
-- test 20: koszulComplexDGM over regular ring vs complete intersection
-- Regular ring: higher Koszul-module homology vanishes.
S1 = QQ[x, y, z]
KM1 = koszulComplexDGM S1^1
C1 = toComplex KM1
assert(prune HH_1 C1 == 0)
assert(prune HH_2 C1 == 0)
assert(prune HH_3 C1 == 0)
-- Complete intersection: higher Koszul-module homology is nonzero.
S2 = QQ[x, y] / ideal(x^2, y^2)
KM2 = koszulComplexDGM S2^1
C2 = toComplex KM2
assert(prune HH_1 C2 != 0)
assert(prune HH_2 C2 != 0)
///

TEST ///
-- test 21: dgModuleMap mult-by-y on semifree resolution of residue field
-- Over R = k[x,y]/(x^2,y^2), multiplying by y annihilates the residue field,
-- so the induced chain map has zero action on all homology.
R = QQ[x, y] / ideal(x^2, y^2)
A = koszulComplexDGA R
Mdg = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 3)
natGens = apply(rank Mdg.natural, i -> (Mdg.natural)_i)
fy = dgModuleMap(Mdg, Mdg, apply(natGens, g -> y * g))
assert(isWellDefined fy)
cmy = toComplexMap fy
assert(all(0..3, n -> prune HH_n cmy == 0))
-- Matrix form also works for the identity.
idmat = id_(Mdg.natural)
g = dgModuleMap(Mdg, Mdg, idmat)
assert(isWellDefined g)
assert(g == identityDGModuleMap Mdg)
///

TEST ///
-- test 22: identityDGModuleMap and zeroDGModuleMap composition laws
R = ZZ/101[x, y] / ideal(x^2, y^2)
A = koszulComplexDGA R
KM = koszulComplexDGM R^1
-- id_KM is the identity DGModuleMap; also compares equal to 1.
assert(id_KM == identityDGModuleMap KM)
assert(id_KM == 1)
-- Composition with mult-by-y on a semifree resolution.
Mdg = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 2)
natGens = apply(rank Mdg.natural, i -> (Mdg.natural)_i)
fy = dgModuleMap(Mdg, Mdg, apply(natGens, v -> y * v))
idMdg = identityDGModuleMap Mdg
assert(idMdg * fy == fy)
assert(fy * idMdg == fy)
assert(idMdg * idMdg == idMdg)
-- Zero map: neutral for +, absorbing for *.
zM = zeroDGModuleMap(Mdg, Mdg)
assert(idMdg + zM == idMdg)
assert(zM * idMdg == zM)
assert(idMdg * zM == zM)
-- Cokernel of zero recovers the module (rank equality).
Q = cokernel zM
assert(rank Q.natural == rank Mdg.natural)
K = kernel zM
assert(instance(K, DGSubmodule))
///

TEST ///
-- test 23: dgSubmodule d-closure on a minimal semifree resolution
R = QQ[x, y] / ideal(x^2, y^2)
A = koszulComplexDGA R
Mdg = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 3)
-- Seeding with hom-degree-2 gens; d-closure pulls in hom-degree-4 gens.
pos2 = positions(Mdg.Degrees, d -> d#0 == 2)
seedHi = (id_(Mdg.natural))_pos2
Shi = dgSubmodule(Mdg, seedHi)
assert(isWellDefined Shi)
assert(maxDegree Shi == 4)
-- Higher-degree gens are pulled in by d-closure (dim > seed dim).
assert(rank (inclusion Shi).source > numcols seedHi)
///

TEST ///
-- test 24: dgIdeal d-saturation of a non-cycle seed
R = QQ[x, y]
A = koszulComplexDGA R
T1 = (A.natural)_0
T2 = (A.natural)_1
-- T_1 * T_2 is not a cycle: d(T_1*T_2) = -y*T_1 + x*T_2.
dz = polyDifferential(A, T1 * T2)
assert(dz != 0)
J = dgIdeal(A, {T1 * T2})
-- d-saturation should include both T_1 * T_2 and its differential.
mg = mingens J.natural
assert(numcols mg == 2)
-- Unit and zero edge-cases.
Z = dgIdeal(A, {})
assert(isZero Z)
U = dgIdeal(A, {1_(A.natural), x_(A.natural)})
assert(U.natural == ideal 1_(A.natural))
///

TEST ///
-- test 25: DGSubmodule / DGQuotientModule types and accessors
R = ZZ/101[x]
A = koszulComplexDGA R
M = freeDGModule(A, {0})
S = dgSubmodule(M, id_(M.natural))
assert(ambient S === M)
assert(source inclusion S === S)
assert(target inclusion S === M)
Z = dgSubmodule(M, {})
Q = M / Z
assert(instance(Q, DGQuotientModule))
assert(ambient Q === M)
assert(subDGModule Q === Z)
assert(source projection Q === M)
assert(target projection Q === Q)
-- freeDGModule with multigraded ring
Rg = ZZ/101[x, y, Degrees => {{1, 0}, {0, 1}}]
Ag = koszulComplexDGA Rg
Mg = freeDGModule(Ag, {{0, 0, 0}, {1, 0, 0}})
assert(#degrees Mg == 2)
-- dgQuotientModule explicit constructor
Qexp = dgQuotientModule(M, Z)
assert(instance(Qexp, DGQuotientModule))
assert(ambient Qexp === M)
assert(subDGModule Qexp === Z)
-- full-sub quotient is zero
Sfull = dgSubmodule(M, id_(M.natural))
assert(isZero (M / Sfull))
-- ambient DGIdeal
I = dgIdeal(A, {x_(A.natural)})
assert(ambient I === A)
///

TEST ///
-- test 26: module-like API on DG modules (numgens/rank/degrees/isHomogeneous/isFreeDGModule/isZero)
R = ZZ/101[x, y]
A = koszulComplexDGA R
Anat = A.natural
M = freeDGModule(A, {0, 1, 2})
assert(numgens M == 3)
assert(rank M == 3)
assert(degrees M == {{0,0},{1,0},{2,0}})
assert(isHomogeneous M)
assert(isFreeDGModule M)
assert(not isZero M)
-- Submodule accessors
M1 = freeDGModule(A, {0})
S = dgSubmodule(M1, matrix {{x_Anat, y_Anat}})
assert(numgens S == 2)
assert(isHomogeneous S)
Q = M1 / S
assert(numgens Q == numgens M1)
assert(isHomogeneous Q)
assert(super S === M1)
assert(super Q === M1)
assert(cover Q === M1)
assert(numcols relations Q == 2)
-- Zero module
M0 = freeDGModule(A, {})
assert(isZero M0)
assert(numgens M0 == 0)
Zsub = dgSubmodule(M1, map(M1.natural, Anat^0, 0))
assert(isZero Zsub)
Full = dgSubmodule(M1, id_(M1.natural))
assert(isZero (M1 / Full))
-- Free-after-tensor
MNK = (M ** R^2) ** R^2
assert(isFreeDGModule MNK)
///

TEST ///
-- test 27: DGSubmodule operations (+, intersect, ==, isSubset)
R = ZZ/101[x, y]
A = koszulComplexDGA R
Anat = A.natural
M = freeDGModule(A, {0})
S = dgSubmodule(M, matrix {{x_Anat}})
T = dgSubmodule(M, matrix {{y_Anat}})
ST = S + T
assert(isWellDefined ST)
assert(isSubset(S, ST) and isSubset(T, ST))
assert((S + S) == S)
-- intersect
T2 = dgSubmodule(M, matrix {{x_Anat * y_Anat}})
cap = intersect(S, T2)
assert(isWellDefined cap)
assert(isSubset(cap, S) and isSubset(cap, T2))
Zsub = dgSubmodule(M, map(M.natural, Anat^0, 0))
assert(isZero intersect(S, Zsub))
-- equality/inclusion
U = dgSubmodule(M, matrix {{x_Anat, y_Anat}})
assert(isSubset(S, U))
assert(not isSubset(U, S))
assert(S == S)
-- Different ambient => not equal
M' = freeDGModule(A, {0})
S' = dgSubmodule(M', matrix {{x_Anat}})
assert(not (S == S'))
///

TEST ///
-- test 28: image, kernel, cokernel of DGModuleMap
R = ZZ/101[x]
A = koszulComplexDGA R
M = freeDGModule(A, {0, 1})
natGens = apply(rank M.natural, i -> (M.natural)_i)
setDiff(M, {0, x * natGens#0})
idM = identityDGModuleMap M
zM = zeroDGModuleMap(M, M)
-- image
imId = image idM
imZ = image zM
assert(isWellDefined imId)
assert(isWellDefined imZ)
assert(numcols (inclusion imId).natural == rank M.natural)
assert(numcols (inclusion imZ).natural == 0)
-- image of inclusion recovers the submodule
Sfull = dgSubmodule(M, id_(M.natural))
assert((image (inclusion Sfull)) == Sfull)
-- kernel
kerId = kernel idM
kerZ = kernel zM
assert(isWellDefined kerId)
assert(isWellDefined kerZ)
assert(numcols (inclusion kerId).natural == 0)
assert(numcols (inclusion kerZ).natural == rank M.natural)
assert(isZero kernel (inclusion Sfull))
-- cokernel
assert(isZero cokernel idM)
Q = cokernel zM
assert(ambient Q === M)
assert(subDGModule Q == image zM)
assert(isZero cokernel (inclusion Sfull))
///

TEST ///
-- test 29: homology of DG modules and DG module maps
R = QQ[x, y]/ideal(x^2, y^2)
A = koszulComplexDGA R
k = R^1 / ideal(x, y)
M = minimalSemifreeResolution(A, k, EndDegree => 2)
H0 = prune homology(0, M)
assert(numgens H0 == 1)
idM = identityDGModuleMap M
h0 = homology(idM, 0)
assert(h0 == id_(source h0))
-- Regular ring: higher Koszul-module homology vanishes.
RR1 = QQ[x, y, z]
KM = koszulComplexDGM RR1^1
assert(numgens prune homology(0, KM) == 1)
assert(prune homology(1, KM) == 0)
assert(prune homology(2, KM) == 0)
-- homology functor on DG module
HM = homology M
assert(ring HM === HH A)
HM' = homologyModule M
assert(HM == HM')
-- homology of mult-by-x is zero on resolution of residue field
natGens = apply(rank M.natural, i -> (M.natural)_i)
xMap = dgModuleMap(M, M, apply(natGens, g -> x * g))
h0x = homology(xMap, 0)
assert(h0x == map(target h0x, source h0x, 0))
-- zero map induces zero homology
zM = zeroDGModuleMap(M, M)
h0z = homology(zM, 0)
assert(h0z == map(target h0z, source h0z, 0))
///

TEST ///
-- test 30: homology of DGQuotientModule and homologyClass
R = ZZ/101[x]
A = koszulComplexDGA R
M = freeDGModule(A, {0, 1})
natGens = apply(rank M.natural, i -> (M.natural)_i)
setDiff(M, {0, x * natGens#0})
S = dgSubmodule(M, (id_(M.natural))_{1})
Q = M / S
h0 = prune homology(0, Q)
h1 = prune homology(1, Q)
h2 = prune homology(2, Q)
assert(numgens h0 == 1)
assert(numgens h1 == 1)
assert(h2 == 0)
HQ = homology Q
assert(ring HQ === HH A)
HQ' = homologyModule Q
assert(HQ == HQ')
-- homologyClass: cycle is nonzero, boundary is zero
z = natGens#0
H = prune homology(0, M)
hc = homologyClass(M, z)
assert(hc != 0_H)
b = x * natGens#0
hb = homologyClass(M, b)
assert(hb == 0_(prune homology(0, M)))
///

TEST ///
-- test 31: prune / minimalPresentation
R = ZZ/101[x, y]
A = koszulComplexDGA R
Anat = A.natural
M = freeDGModule(A, {0})
-- Redundant seed pruned to one generator.
S = dgSubmodule(M, matrix {{1_Anat, x_Anat, y_Anat}})
Sp = prune S
assert(numcols (inclusion S).natural == 3)
assert(numcols (inclusion Sp).natural == 1)
assert(image (inclusion S).natural == image (inclusion Sp).natural)
assert(isWellDefined Sp)
Sp' = minimalPresentation S
assert(numcols (inclusion Sp').natural == 1)
-- prune on quotient
Q = M / S
Qp = prune Q
assert(image (inclusion Qp.subDGModule).natural == image (inclusion S).natural)
Qp' = minimalPresentation Q
assert(image (inclusion Qp'.subDGModule).natural == image (inclusion S).natural)
-- prune on DGModule / DGAlgebra / maps is identity
Mfree = freeDGModule(A, {0, 1})
assert((prune Mfree) === Mfree)
assert((minimalPresentation Mfree) === Mfree)
assert((prune A) === A)
assert((minimalPresentation A) === A)
phi = identityDGAlgebraMap A
assert((prune phi) === phi)
assert((minimalPresentation phi) === phi)
idMp = identityDGModuleMap Mfree
assert((prune idMp) === idMp)
assert((minimalPresentation idMp) === idMp)
///

TEST ///
-- test 32: predicates (isWellDefined, isAcyclic, isQuasiIsomorphism, annihilator)
R = QQ[x, y, z]
A = koszulComplexDGA R
assert(isWellDefined A)
assert(isAcyclic A)
-- Acyclic on a DG module (Koszul on regular ring)
KM = koszulComplexDGM R^1
assert(isAcyclic KM)
-- Non-acyclic on c.i.
Rci = QQ[x, y]/ideal(x^2, y^2)
KMci = koszulComplexDGM Rci^1
assert(not isAcyclic(KMci, EndDegree => 3))
-- Identity DGAlgebraMap is quasi-iso
phi = identityDGAlgebraMap A
assert(isWellDefined phi)
assert(isQuasiIsomorphism phi)
-- Submodule/quotient well-definedness
Anat = A.natural
Mfree = freeDGModule(A, {0})
Sub = dgSubmodule(Mfree, matrix {{1_Anat}})
Quo = Mfree / Sub
assert(isWellDefined Sub)
assert(isWellDefined Quo)
-- isWellDefined on DGModuleMap
Aci = koszulComplexDGA Rci
Mdg = minimalSemifreeResolution(Aci, Rci^1 / ideal(x, y), EndDegree => 2)
idM = identityDGModuleMap Mdg
zM = zeroDGModuleMap(Mdg, Mdg)
assert(isWellDefined idM)
assert(isWellDefined zM)
-- annihilator of submodule
A2 = koszulComplexDGA (ZZ/101[x, y])
A2nat = A2.natural
M2 = freeDGModule(A2, {0})
Sx = dgSubmodule(M2, matrix {{x_A2nat}})
Ian = annihilator Sx
assert(isWellDefined Ian)
S0 = dgSubmodule(M2, map(M2.natural, A2nat^0, 0))
assert(annihilator S0 == dgIdeal(A2, {1_A2nat}))
-- annihilator of quotient
Q2 = M2 / Sx
IQ = annihilator Q2
assert(isWellDefined IQ)
assert(isSubset(dgIdeal(A2, {x_A2nat}), IQ))
-- quotient acyclicity edge case
R0 = ZZ/101[x]
A0 = koszulComplexDGA R0
M0 = freeDGModule(A0, {0, 1})
natGens0 = apply(rank M0.natural, i -> (M0.natural)_i)
setDiff(M0, {0, x * natGens0#0})
Sfull0 = dgSubmodule(M0, id_(M0.natural))
assert(isAcyclic (M0 / Sfull0))
///

TEST ///
-- test 33: koszulComplexDGM, adjoinGenerators, killCycles
R = QQ[x, y]
A = koszulComplexDGA R
KM = koszulComplexDGM(A, R^1 / ideal(x))
assert(KM.dgAlgebra === A)
-- adjoinGenerators extends degree list
R2 = QQ[x]
A2 = koszulComplexDGA R2
M = freeDGModule(A2, {0})
natGens = apply(rank M.natural, i -> (M.natural)_i)
Mnew = adjoinGenerators(M, {x * natGens#0})
assert(#Mnew.Degrees == 2)
assert(first Mnew.Degrees#1 == 1)
-- killCycles on a DG module
M2k = adjoinGenerators(M, {x * natGens#0})
assert(prune homology(0, M2k) != 0)
M3 = killCycles(M2k, StartDegree => 1, EndDegree => 2)
assert(prune homology(1, M3) == 0)
///

TEST ///
-- test 34: semifreeResolution / minimalSemifreeResolution / isMinimalSemifreeResolution
R = QQ[x]
A = koszulComplexDGA R
M = R^1 / ideal(x)
Mdg = semifreeResolution(A, M, EndDegree => 3)
assert(prune homology(0, Mdg) != 0)
assert(all(1..3, n -> prune homology(n, Mdg) == 0))
-- 2-arg form w/ ambient ring
Mdg' = semifreeResolution(M, EndDegree => 2)
assert(Mdg'.ring === R)
assert(Mdg'.dgAlgebra.ring === R)
-- Minimal resolution over c.i.
Rci = QQ[x, y] / ideal(x^2, y^2)
kci = Rci^1 / ideal(x, y)
Aci = koszulComplexDGA Rci
Mmin = minimalSemifreeResolution(Aci, kci, EndDegree => 3)
assert(isMinimalSemifreeResolution Mmin)
assert(all(1..3, n -> prune homology(n, Mmin) == 0))
-- Ranks 1,2,3,4 at degrees 0,1,2,3
assert(toList apply(0..3, n -> numcols moduleDifferential(n, Mmin)) == {1, 2, 3, 4})
-- 2-arg minimal
Mm2 = minimalSemifreeResolution(M, EndDegree => 2)
assert(Mm2.ring === R)
assert(isMinimalSemifreeResolution Mm2)
-- isMinimalSemifreeResolution negative
Mbad = freeDGModule(A, {0, 1})
natGens = apply(rank Mbad.natural, i -> (Mbad.natural)_i)
setDiff(Mbad, {0, natGens#0})  -- diff(e_1) = e_0 alone, not minimal
assert(not isMinimalSemifreeResolution Mbad)
///

TEST ///
-- test 35: polyDifferential, polyDiffMonomial, dgComplex, isValidDGModule, isDGSubmodule
R = QQ[x, y, z]
A = koszulComplexDGA R
-- d^2 = 0 on Koszul resolution of regular ring
d1 = polyDifferential(1, A)
d2 = polyDifferential(2, A)
d3 = polyDifferential(3, A)
assert(d1 * d2 == 0)
assert(d2 * d3 == 0)
-- degree 0: R^1 -> R^0
Rci = QQ[x, y] / ideal(x^2, y^2)
Aci = koszulComplexDGA Rci
d0 = polyDifferential(0, Aci)
assert(source d0 == Rci^1 and target d0 == Rci^0)
-- element form and Leibniz
gsci = gens Aci.natural
rpd = polyDiffMonomial(Aci, 0_(Aci.natural))
assert(rpd == 0_(Aci.natural))
r2 = polyDiffMonomial(Aci, gsci#0 * gsci#1)
assert(r2 == x * gsci#1 - y * gsci#0)
-- dgComplex caching
C1 = dgComplex Aci
C2 = dgComplex Aci
assert(C1 === C2)
invalidateDGAlgebraCache Aci
assert(not Aci.cache#?(symbol dgComplex))
C3 = dgComplex Aci
assert(instance(C3, Complex))
-- dgComplex over infinite DGAlgebra (acyclicClosure)
Acc = acyclicClosure(Rci, EndDegree => 2)
assert(instance(dgComplex Acc, Complex))
-- dgComplex on DGModule (caching)
KMci = koszulComplexDGM Rci^1
D1 = dgComplex KMci
D2 = dgComplex KMci
assert(D1 === D2)
assert(isValidDGModule KMci)
-- isDGSubmodule: image and kernel of identityDGModuleMap are d-stable
R0 = ZZ/101[x]
A0 = koszulComplexDGA R0
Mdg = freeDGModule(A0, {0, 1})
natGens0 = apply(rank Mdg.natural, i -> (Mdg.natural)_i)
setDiff(Mdg, {0, x * natGens0#0})
idM = identityDGModuleMap Mdg
imF = image idM
assert(isDGSubmodule(target idM, (inclusion imF).natural))
kerF = kernel idM
assert(isDGSubmodule(source idM, (inclusion kerF).natural))
///

TEST ///
-- test 36: getBoundaryPreimage
R = QQ[x, y] / ideal(x^2, y^2)
A = koszulComplexDGA R
M = freeDGModule(A, {0, 1})
natGens = apply(rank M.natural, i -> (M.natural)_i)
setDiff(M, {0, x * natGens#0})
b = x * natGens#0
(ok, pre) = getBoundaryPreimage(M, b)
assert(ok === true)
assert(diff(M, pre) == b)
-- non-boundary case
M2 = freeDGModule(A, {0})
ng2 = apply(rank M2.natural, i -> (M2.natural)_i)
(ok2, residue) = getBoundaryPreimage(M2, ng2#0)
assert(ok2 === false)
assert(residue != 0)
-- list form: zero + boundary
R3 = QQ[x]
A3 = koszulComplexDGA R3
M3 = freeDGModule(A3, {0, 1})
natGens3 = apply(rank M3.natural, i -> (M3.natural)_i)
setDiff(M3, {0, x * natGens3#0})
b3 = x * natGens3#0
(okL, lifts) = getBoundaryPreimage(M3, {b3, 0_(M3.natural)})
assert(okL === true)
assert(diff(M3, lifts#0) == b3)
assert(diff(M3, lifts#1) == 0)
///

TEST ///
-- test 37: moduleDifferential, diff(DGModule/DGQuotientModule, Vector), displays
R = QQ[x, y] / ideal(x^2, y^2)
k = R^1 / ideal(x, y)
A = koszulComplexDGA R
Mdg = minimalSemifreeResolution(A, k, EndDegree => 2)
d1 = moduleDifferential(1, Mdg)
d2 = moduleDifferential(2, Mdg)
assert(d1 * d2 == 0)
-- (diff, DGModule, Vector)
R2 = QQ[x]
A2 = koszulComplexDGA R2
M = freeDGModule(A2, {0, 1})
natGens = apply(rank M.natural, i -> (M.natural)_i)
setDiff(M, {0, x * natGens#0})
assert(diff(M, natGens#1) == x * natGens#0)
assert(diff(M, natGens#0) == 0_(M.natural))
-- (diff, DGQuotientModule, Vector)
R3 = ZZ/101[x]
A3 = koszulComplexDGA R3
M3 = freeDGModule(A3, {0, 1})
natGens3 = apply(rank M3.natural, i -> (M3.natural)_i)
setDiff(M3, {0, x * natGens3#0})
S = dgSubmodule(M3, (id_(M3.natural))_{1})
Q = M3 / S
assert(instance(projection Q, DGModuleMap))
assert(diff(Q, (Q.natural)_0) == 0_(Q.natural))
-- generatorTable, dgModuleSummary, displayModuleBlockDiff return a Net
assert(instance(generatorTable M, Net))
assert(instance(dgModuleSummary(Mdg, 3), Net))
-- moduleBlockDiff columns
Mdg3 = minimalSemifreeResolution(A, k, EndDegree => 3)
b2 = moduleBlockDiff(Mdg3, 2)
assert(matrix b2 == moduleDifferential(2, Mdg3))
srcIdx = indices source b2
assert(all(srcIdx, l -> #l == 2 and instance(l#0, ZZ) and instance(l#1, List)))
assert(instance(displayModuleBlockDiff(Mdg, 2), Net))
///

TEST ///
-- test 38: manipulation (baseChange / subDGAlgebra / restrictDifferential / truncateGenerators / killHomologyAtDegree)
R = QQ[x, y]
S = R / ideal(x^2, y^2)
A = koszulComplexDGA R
B = baseChange(A, S)
assert(underlyingRing B === S)
assert(isWellDefinedDifferential B)
-- RingMap form
phi = map(R, R, {x, 0})
Bphi = baseChange(A, phi)
assert(underlyingRing Bphi === R)
-- subDGAlgebra / restrictDifferential / truncateGenerators
Ac = koszulComplexDGA (QQ[x, y] / ideal(x^2, y^2))
Bsub = subDGAlgebra(Ac, {0})
assert(numgens Bsub.natural == 1)
Brd = restrictDifferential(Ac, {0})
assert(numgens Brd.natural == 1)
Btr = truncateGenerators(Ac, 0)
assert(numgens Btr.natural == numgens Ac.natural)
-- subDGAlgebra must receive a d-closed sub-list
Rx = QQ[x] / ideal(x^2)
Ax = koszulComplexDGA Rx
Ax' = adjoinVariables(Ax, {x * (gens Ax.natural)#0})
errored = try (subDGAlgebra(Ax', {1}); false) else true
assert(errored)
-- killHomologyAtDegree
Areg = koszulComplexDGA (QQ[x, y])
assert(killHomologyAtDegree(Areg, 1) === Areg)  -- no homology in deg 1
Bkill = killHomologyAtDegree(Ac, 1)
assert(numgens Bkill.natural > numgens Ac.natural)
assert(prune homology(1, Bkill) == 0)
-- isValidDGAlgebra / isWellDefinedDifferential
assert(isValidDGAlgebra Ac)
assert(isWellDefinedDifferential Ac)
Mfree = freeDGModule(Ac, {0})
assert(isWellDefinedDifferential Mfree)
///

TEST ///
-- test 39: accessors (underlyingRing, underlyingAlgebra, differential, generatorDegrees, caching)
R = QQ[x, y] / ideal(x^2, y^2)
A = koszulComplexDGA R
assert(underlyingRing A === R)
M = koszulComplexDGM R^1
assert(underlyingRing M === R)
assert(underlyingAlgebra A === A.natural)
assert(underlyingAlgebra M === M.natural)
assert(differential A === A.diff)
-- generatorDegrees returns whatever the DGA records
assert(generatorDegrees A == A.Degrees)
Mfree = freeDGModule(A, {0, 1, 3})
assert(generatorDegrees Mfree == Mfree.Degrees)
-- Caches
ensureDGAlgebraCaches A
assert(instance(A.cache, CacheTable))
ensureDGAlgebraCaches A
assert(instance(A.cache, CacheTable))
ensureDGAlgebraCaches M
assert(instance(M.cache, CacheTable))
-- invalidateDGAlgebraCache resets cache slots
C1 = dgComplex A
invalidateDGAlgebraCache A
assert(not A.cache#?(symbol dgComplex))
C2 = dgComplex A
assert(instance(C2, Complex))
D1 = dgComplex M
invalidateDGAlgebraCache M
assert(not M.cache#?(symbol dgComplex))
D2 = dgComplex M
assert(instance(D2, Complex))
///

TEST ///
-- test 40: extensive DGIdeal operations (+, *, ^, intersect, :, %, quotient, predicates)
R = ZZ/101[x, y]
A = koszulComplexDGA R
Anat = A.natural
I = dgIdeal(A, {x_Anat})
J = dgIdeal(A, {y_Anat})
assert isWellDefined I
assert isDGIdeal(A, I.natural)
-- +, *, ^, intersect, :
assert isWellDefined(I + J)
assert(I + J == J + I)
Zp = dgIdeal(A, {})
assert(Zp + I == I)
assert isWellDefined(I * J)
assert(I * J == J * I)
Uone = dgIdeal(A, {1_Anat})
assert(Uone * I == I)
I5 = dgIdeal(A, {x_Anat, y_Anat})
assert isWellDefined(I5^2)
assert isWellDefined(I5^3)
assert(I5^2 * I5 == I5^3)
I5zero = I5^0
assert(I5zero.natural == ideal 1_Anat)
assert isWellDefined(Zp^0)
assert isWellDefined(Zp^5)
assert isWellDefined intersect(I, J)
ZI = intersect(Zp, I)
assert(numgens ZI.natural == 0)
Ic = dgIdeal(A, {x_Anat * y_Anat})
Jc = dgIdeal(A, {y_Anat})
Kc = Ic : Jc
assert isWellDefined Kc
-- isSubset, ==, %
Ia = dgIdeal(A, {x_Anat})
Ja = dgIdeal(A, {x_Anat, y_Anat})
assert isSubset(Ia, Ja)
assert(Ia != Ja)
assert(Ia == Ia)
assert((x_Anat^2 % I) == 0)
assert((y_Anat^2 % I) == y_Anat^2)
-- Accessors
Im = dgIdeal(A, {x_Anat, x_Anat * y_Anat})
ignore = mingens Im
ignore = numgens Im
ignore = module Im
assert(numgens Im == numgens Im.natural)
assert(ring Im === R)
assert(ambient Im === A)
-- prune
Ipr = dgIdeal(A, {x_Anat, x_Anat * y_Anat, x_Anat^2})
Jpr = prune Ipr
assert isWellDefined Jpr
assert(Jpr.natural == Ipr.natural)
-- quotient A / I
Rq = ZZ/101[x]
Aq = koszulComplexDGA Rq
Tq = (Aq.natural)_0
Iq = dgIdeal(Aq, {Tq})
Bq = Aq / Iq
assert(class Bq === DGAlgebra)
assert isWellDefined Bq
-- predicates
Ih = dgIdeal(A, {x_Anat, y_Anat})
assert isHomogeneous Ih
assert not isZero Ih
Zh = dgIdeal(A, {})
assert isZero Zh
-- isDGIdeal positive/negative
assert isDGIdeal(A, ideal(x_Anat, y_Anat))
Tsym = Anat_0  -- T_{1,1}, with d(T) = x
assert not isDGIdeal(A, ideal(Tsym))
JT = dgIdeal(A, {Tsym})
assert isDGIdeal(A, JT.natural)
-- zero-seed isZero
Z' = dgIdeal(A, {0_Anat, 0_Anat})
assert isZero Z'
///

TEST ///
-- test 41: export layer (setDiff DGModule, maxDegree, getBasis, toComplex, toComplexMap)
R = QQ[x]
A = koszulComplexDGA R
M = freeDGModule(A, {0, 1})
natGens = apply(rank M.natural, i -> (M.natural)_i)
setDiff(M, {0, x^2 * natGens#0})
d1 = moduleDifferential(1, M)
d2 = moduleDifferential(2, M)
assert(d1 * d2 == 0)
-- maxDegree on DGModule matches its ambient DGAlgebra
Rci = QQ[x, y] / ideal(x^2, y^2)
KM = koszulComplexDGM Rci^1
assert(maxDegree KM == maxDegree KM.dgAlgebra)
-- maxDegree on DGQuotientModule matches ambient module
R3 = ZZ/101[x]
A3 = koszulComplexDGA R3
M3 = freeDGModule(A3, {0, 1})
zM3 = zeroDGModuleMap(M3, M3)
Q3 = cokernel zM3
assert(maxDegree Q3 == maxDegree M3)
-- getBasis on Koszul DG module
b = getBasis(1, KM)
assert(instance(b, Matrix))
assert(numcols b == 2)
-- toComplex on DGModule
C = toComplex KM
assert(instance(C, Complex))
assert(C.dd_1 == moduleDifferential(1, KM))
assert(C.dd_2 == moduleDifferential(2, KM))
-- toComplex on DGModule with degree cap
KMr = koszulComplexDGM (QQ[x, y, z])^1
Cr = toComplex(KMr, 2)
assert(instance(Cr, Complex))
assert(max Cr == 2)
-- toComplex on DGQuotientModule
CM = toComplex M3
CQ = toComplex Q3
assert(rank CM_0 == rank CQ_0)
assert(rank CM_1 == rank CQ_1)
-- toComplex on DGIdeal: errors (intended)
Aid = koszulComplexDGA (ZZ/101[x])
Tid = (gens Aid.natural)#0
Iid = dgIdeal(Aid, {Tid})
errored = try (toComplex Iid; false) else true
assert(errored)
-- toComplexMap on DGAlgebraMap
Rcu = QQ[u, v] / ideal(u^2, v^2)
Acu = koszulComplexDGA Rcu
phi = dgAlgebraMap(Acu, Acu, matrix {gens Acu.natural})
cm = toComplexMap phi
assert(instance(cm, ComplexMap))
-- toComplexMap on DGModuleMap identity at each degree
Rc2 = QQ[x, y] / ideal(x^2, y^2)
Mdg = minimalSemifreeResolution(koszulComplexDGA Rc2, Rc2^1 / ideal(x, y), EndDegree => 3)
idMdg = identityDGModuleMap Mdg
assert(all(0..3, n -> (
    cmn := toComplexMap(idMdg, n);
    source cmn == target cmn and cmn == id_(source cmn)
    )))
///
