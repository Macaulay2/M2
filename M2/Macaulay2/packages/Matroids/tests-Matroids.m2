
TEST ///
M = matroid({a, b, c, d}, {{a, b}, {a, c}})
assert(isWellDefined M and not isSimple M)
assert(set bases M === set {set{0, 1}, set{0, 2}})
assert(set nonbases M === set {set {2, 3}, set {0, 3}, set {1, 3}, set {1, 2}})
assert(set circuits M === set {set {1, 2}, set {3}})
assert(M == matroid({a,b,c,d},{{b,c},{d}}, EntryMode => "circuits"))
assert(not isDependent(M, {b}))
assert(set independentSets_M 1 === set {set{0}, set{1}, set{2}})
assert(coloops M === {0})
assert(loops M === {3})
assert(rank_M {a,d} === 1)
assert(closure_M {c,d} === {1, 2, 3})
assert(set hyperplanes M === set {set {0, 3}, set {1, 2, 3}})
assert(set flats M === set {set {3}, set {0, 3}, set {1, 2, 3}, set {0, 1, 2, 3}})
assert(values fVector M === {1, 2, 1})
D = dual M
assert(D == dual M)
N1 = M \ {d}
assert((N1_*, set bases N1) === ({a, b, c}, set {set {0, 1}, set {0, 2}}))
N2 = M / set{1}
assert((N2_*, set bases N2) === ({a, c, d}, set {set {0}}))
MA = matroid matrix{{0,4,-1,6},{0,2/3,7,1}}
assert(areIsomorphic(MA, M))
///

TEST ///
assert(not isWellDefined matroid({a,b,c,d}, {{b,c},{b,d}}, EntryMode=>"nonbases"))
M = matroid({a,b,c,d}, {}, EntryMode => "nonbases")
assert(isWellDefined M)
assert(ideal M == 0)
assert(M == matroid({a,b,c,d}, {}, EntryMode => "circuits"))
assert(M == uniformMatroid(4,4))
assert(#bases M == 1)
assert((try fundamentalCircuit(M, set{1,2}, 3)) === null)
R = ZZ/101[x_0..x_3]
assert(M == matroid monomialIdeal 0_R)
assert((try matroid ideal 1_R) === null)
assert((try matroid ideal()) === null)
N = matroid({a,b,c,d}, {{}})
assert(rank N == 0 and isWellDefined N and N == dual M)
M = matroid matrix{{1,0,1,1},{0,1,1,1}}
assert(M \ set{0} == M \ set{1} and not M \ set{0} == M \ set{2})
assert(fundamentalCircuit (M, (bases M)#2, 3) === set{2, 3})
assert(fundamentalCircuit (M, M_{0,1}, M_3) === set{0,1,3})
assert(try fundamentalCircuit (M, M_{1,2}, M_3) else null === null)
assert(toString tuttePolynomial M == "x^2+x*y+y^2+x+y")
///

TEST ///
S = uniformMatroid(2,4) ++ matroid completeGraph 3
assert(S == uniformMatroid(2,4) + matroid completeGraph 3)
C = components S
assert(S == C#0 ++ C#1)
G = graph({{0,1},{1,2},{0,2},{3,4},{4,5},{3,5}})
assert(#connectedComponents getRepresentation matroid G == 2)
M = matroid(G, Loops => {0,3,5})
assert(#loops M == 3)
C = components M
assert(#C == 5 and #getIsos(M, fold(C, (a, b) -> a ++ b)) == 432)
assert(characteristicPolynomial M == 0)
M0 = matroid({a,b,c,d}, {{a},{b},{c}})
M1 = matroid({a,b,c,d}, {{b},{c},{d}})
assert(M0 + M1 == uniformMatroid(2,4))
F7 = specificMatroid "fano"
NF = specificMatroid "nonfano"
assert(all({F7 + NF, F7 + F7, NF + NF}, M -> M == uniformMatroid(6, 7)))
///

TEST ///
G = graph({{0,1},{1,2},{2,3},{3,4},{4,5},{5,6},{6,0},{0,2},{0,3},{0,4},{1,3},{3,5},{3,6}})
M = matroid G
assert(isConnected M)
assert(not is3Connected M)
///

TEST ///
M5 = matroid completeGraph 5
U24 = uniformMatroid(2, 4)
M4 = matroid completeGraph 4
assert(#bases M5 === 125 and #bases U24 == 6)
assert(set getIsos(U24, dual U24) === set permutations 4)
assert(hasMinor(M5, M4))
minorM5 = minor(M5, set{9}, set{3,5,8})
assert(areIsomorphic(minorM5, M4))
assert(not hasMinor(M5, U24))
///

TEST ///
K4 = completeGraph 4
M4 = matroid K4
assert(toString tuttePolynomial M4 === "x^3+y^3+3*x^2+4*x*y+3*y^2+2*x+2*y")
assert(tutteEvaluate(M4, 2, 1) === 38)
assert(getRepresentation M4 === K4)
B={{0,1,2},{0,1,3},{0,1,4},{0,1,5},{0,2,3},{0,2,5},{0,3,4},{0,4,5},{1,2,3},{1,2,4},{1,3,5},{1,4,5},{2,3,4},{2,3,5},{2,4,5},{3,4,5}}
altM = matroid(toList(0..5),B)
assert(areIsomorphic(altM, M4))
assert(hasMinor(altM,M4))
A = random(ZZ^3,ZZ^5)
assert(getRepresentation matroid A === A)
///

TEST ///
U34 = uniformMatroid(3,4)
I = idealChowRing U34
assert((0..<rank U34)/(i -> numColumns basis(i, comodule I)) === (1,7,1))
F = cogeneratorChowRing U34
phi = map(ring F, ring I, gens ring F)
assert(0 == diff(gens phi I, F))
///

TEST ///
F7 = specificMatroid "fano"
PG22 = projectiveGeometry(2,2)
A = transpose sub(matrix toList(((3:0)..(3:2-1))/toList), ZZ/2)
assert(PG22 == F7 and areIsomorphic(PG22, simpleMatroid matroid A))
M4 = matroid completeGraph 4
assert(all(F7_*, x -> areIsomorphic(M4, F7 \ {x})))
w = {0, log(2), 4/3, 1, -4, 2, pi_RR}
assert(maxWeightBasis(F7, w) === set{2,5,6})
assert(maxWeightBasis(F7, rsort w) === set{0,1,2})
///

TEST ///
-- no-check-flag #1392
M0 = matroid graph({{a,b},{b,c},{c,d},{d,e},{e,f},{f,g},{f,h},{c,h},{c,f},{a,g},{d,g}})
M1 = matroid graph({{a,b},{b,c},{c,d},{d,e},{e,f},{f,g},{f,h},{c,h},{c,f},{a,g},{a,h}})
T = ZZ[x,y]
assert(isWellDefined M0 and isWellDefined M1)
assert(tuttePolynomial(M0, T) === tuttePolynomial(M1, T))
F1 = set{0,1,2,3,7}
F2 = F1 + set{5,8}
assert(areIsomorphic(uniformMatroid(2,2), minor(M0, F1, M0.groundSet - F2)))
assert(areIsomorphic(M0, matroid graph edges graph M0_*))
Delta = independenceComplex M0
F = fVector Delta
assert(ideal Delta == ideal M0 and F === fVector independenceComplex M1)
assert(F === {1,11,55,164,319,409,324,125})
assert(not areIsomorphic(M0, M1))
///

TEST ///
M = binarySpike 5
N = M \ set{#M_*-1}
assert areIsomorphic(N, dual N)
assert(isomorphism(N, dual N) =!= null)
isos = getIsos(N, dual N);
assert Equation(1920, #isos)
-- Sample of the silenced full sweep (`all(isos, phi -> ...)`, ~420s).
-- Each areIsomorphic(N, relabel_N phi) call is ~0.1s, so the full sweep is
-- prohibitive for routine checks but a small sample protects against
-- regressions in relabel/areIsomorphic interaction.
assert all(take(isos, 8), phi -> areIsomorphic(N, relabel_N phi))
///

TEST ///
R = QQ[x_0..x_6]
M0 = matroid(graph(toList(0..4), {set{0,3},set{0,4},set{1,3},set{1,4},set{2,3},set{2,4}}), ParallelEdges => {set{2,4}})
M1 = matroid ideal(x_0*x_1*x_2*x_3,x_0*x_1*x_2*x_4,x_0*x_1*x_3*x_4,x_0*x_2*x_3*x_4,x_1*x_2*x_3*x_4,x_5*x_6)
assert(betti res ideal M0 === betti res ideal M1)
assert(areIsomorphic(M0, M1) == false)
M3 = matroid ideal (x_0*x_1*x_2,x_0*x_3*x_4,x_1*x_2*x_3*x_4,x_0*x_1*x_3*x_5,x_0*x_2*x_3*x_5,x_1*x_2*x_3*x_5,x_0*x_1*x_4*x_5,x_0*x_2*x_4*x_5,x_1*x_2*x_4*x_5,x_1*x_3*x_4*x_5,x_2*x_3*x_4*x_5,x_0*x_1*x_3*x_6,x_0*x_2*x_3*x_6,x_1*x_2*x_3*x_6,x_0*x_1*x_4*x_6,x_0*x_2*x_4*x_6,x_1*x_2*x_4*x_6,x_1*x_3*x_4*x_6,x_2*x_3*x_4*x_6,x_1*x_5*x_6,x_0*x_2*x_5*x_6,x_0*x_3*x_5*x_6,x_2*x_3*x_5*x_6,x_0*x_4*x_5*x_6,x_2*x_4*x_5*x_6,x_3*x_4*x_5*x_6)
M4 = matroid ideal (x_0*x_1*x_2,x_0*x_3*x_4,x_1*x_2*x_3*x_4,x_0*x_1*x_3*x_5,x_0*x_2*x_3*x_5,x_1*x_2*x_3*x_5,x_0*x_1*x_4*x_5,x_0*x_2*x_4*x_5,x_1*x_2*x_4*x_5,x_1*x_3*x_4*x_5,x_2*x_3*x_4*x_5,x_0*x_1*x_3*x_6,x_0*x_2*x_3*x_6,x_1*x_2*x_3*x_6,x_0*x_1*x_4*x_6,x_0*x_2*x_4*x_6,x_1*x_2*x_4*x_6,x_1*x_3*x_4*x_6,x_2*x_3*x_4*x_6,x_0*x_5*x_6,x_1*x_2*x_5*x_6,x_1*x_3*x_5*x_6,x_2*x_3*x_5*x_6,x_1*x_4*x_5*x_6,x_2*x_4*x_5*x_6,x_3*x_4*x_5*x_6)
assert(betti res ideal M3 === betti res ideal M4 and betti res dual ideal M3 === betti res dual ideal M4)
assert(betti res ideal dual M3 === betti res ideal dual M4 and betti res dual ideal dual M3 === betti res dual ideal dual M4)
assert(areIsomorphic(M3, M4) == false)
///

TEST ///
L8 = allMatroids 8;
(M, N) = (L8#615, L8#616)
assert(areIsomorphic(M, dual M))
assert(not areIsomorphic(N, dual N))
assert(betti res ideal N === betti res ideal dual N)
assert(betti res dual ideal N === betti res dual ideal dual N)
///

TEST ///
G0 = graph(toList(0..5), {{0, 3}, {4, 0}, {0, 5}, {4, 1}, {5, 1}, {5, 2}, {4, 3}, {5, 3}, {4, 5}})
G1 = graph(toList(0..5), {{0, 3}, {4, 0}, {0, 5}, {1, 3}, {4, 1}, {5, 2}, {4, 3}, {5, 3}, {4, 5}})
G2 = graph(toList(0..5), {{0, 2}, {4, 0}, {0, 5}, {1, 3}, {4, 1}, {5, 1}, {4, 2}, {5, 2}, {4, 5}})
(M, N, P) = (G0, G1, G2)/matroid
assert(not(M == N) and not(N == P) and not(M == P))
assert((#getIsos(M,N), #getIsos(N,M)) == (8,8))
T = ZZ[x,y]
assert(tuttePolynomial(M, T) == tuttePolynomial(N, T) and tuttePolynomial(N, T) == tuttePolynomial(P, T))
G = graph({{0,1},{0,2},{1,2},{2,3},{3,4},{4,5},{4,6},{5,6}}) -- bowtie graph
M = matroid G
assert(set coloops M === set {4,3})
p = {0, 4, 5, 1, 2, 3, 6, 7}
assert(values isomorphism (M, relabel(M, p)) === p)
///

TEST ///
AG32 = specificMatroid "AG32"
assert(AG32 == affineGeometry(3,2))
assert(set circuits AG32 === set hyperplanes AG32 and #circuits AG32 == 14)
isos = getIsos(AG32, dual AG32)
assert(#isos == 1344 and member(toList(0..7), isos))
V8plus = specificMatroid "V8+"
assert(V8plus == dual V8plus)
V = specificMatroid "vamos"
assert(V == relaxation(V8plus, set{4,5,6,7}))
isos = getIsos(V, dual V)
assert(#isos == 64 and not member(toList(0..7), isos))
assert(hasMinor(V, uniformMatroid(2,4)))
R10 = specificMatroid "R10"
assert(#getIsos(R10 \ set{0}, matroid completeMultipartiteGraph {3,3}) == 72)
///

TEST ///
P8 = matroid(id_((ZZ/3)^4) | matrix{{0_(ZZ/3),1,1,-1},{1,0,1,1},{1,1,0,1},{-1,1,1,0}})
aut = getIsos (P8, P8) -- automorphism group is transitive
assert(all(subsets(P8.groundSet,2)/toList, s -> any(aut, sigma -> sigma_(s#0) == s#1)))
sigma1 = {7,6,5,4,0,1,2,3}
sigma2 = {1,3,0,2,5,7,4,6}
assert(member(sigma1, aut) and member(sigma2, aut))
S8 = matroid(id_((ZZ/2)^4) | matrix{{0_(ZZ/2),1,1,1},{1,0,1,1},{1,1,0,1},{1,1,1,1}})
F7 = specificMatroid "fano"
assert(#select(S8_*, x -> areIsomorphic(S8 / {x}, F7)) == 1)
assert(#select(S8_*, x -> areIsomorphic(S8 \ {x}, dual F7)) == 1)
assert(#getIsos(F7, F7) == 168)
M = relabel(F7, hashTable {3 => 4, 4 => 6, 6 => 3})
assert(areIsomorphic(F7, M))
assert(areIsomorphic(F7, relabel F7))
///

TEST ///
V = specificMatroid "vamos"
elapsedTime assert not isPositivelyOriented V
elapsedTime assert isPositivelyOrientable V
M = matroid(toList(0..<6), {{0,1,2},{0,3,4},{1,3,5}}, EntryMode => "nonbases")
elapsedTime assert not isPositivelyOrientable M
///

TEST /// -- cf. https://github.com/Macaulay2/M2/issues/2403
G = graph(toList(0..9), {{0,4},{1,4},{2,4},{3,4},{0,6},{5,6},{1,7},{5,7},{1,8},{2,8},{5,8},{1,9},{2,9},{3,9},{5,9}})
assert isConnected G
assert(#edges kruskalSpanningForest G == #vertices G - 1)
assert(#edges spanningForest G == #vertices G - 2)
///

TEST ///
B = specificMatroid betsyRoss
k = GF 4
setRandomSeed 5
elapsedTime A = searchRepresentation(B, k)
assert(matroid A == B)
///

TEST ///
smallMatroids = apply(6, i -> allMatroids i)
assert(smallMatroids/(l -> #l) == {1,2,4,8,17,38})
smallMatroids = flatten smallMatroids
assert(all(smallMatroids, isWellDefined))
assert(not any(subsets(smallMatroids, 2), S -> areIsomorphic(S#0, S#1)))
assert(all(smallMatroids_{1..69}, M -> areIsomorphic(M, fold(components M, (a, b) -> a ++ b))))
///

TEST ///
P = specificMatroid "pappus"
assert Equation(#nonbases P, 9)
NP = specificMatroid "nonpappus"
assert Equation(#nonbases NP, 8)
U36 = uniformMatroid(3,6)
U36minors = allMinors(P, U36)
assert Equation(#U36minors, 3)
BR = specificMatroid "betsyRoss"
elapsedTime assert Equation(hasMinor(BR, U36), true)
///

TEST ///
V8plus = specificMatroid "V8+"
s = "Matroid(groundset = 'abcdefgh', bases = ['dfgh','cfgh','bfgh','afgh','degh','cegh','begh','aegh','bdgh','adgh','bcgh','acgh','defh','cefh','befh','aefh','cdfh','bdfh','adfh','bcfh','acfh','abfh','cdeh','bdeh','adeh','bceh','aceh','abeh','bcdh','acdh','abdh','abch','defg','cefg','befg','aefg','cdfg','bdfg','adfg','bcfg','acfg','abfg','cdeg','bdeg','adeg','bceg','aceg','abeg','bcdg','acdg','abdg','abcg','bdef','adef','bcef','acef','bcdf','acdf','abdf','abcf','bcde','acde','abde','abce'])"
assert (s === toSageMatroid V8plus)
assert (V8plus == fromSageMatroid s)
///

TEST ///
k = GF 2
X = toList(0..<9)
L = apply(subsets X, s -> matroid(id_(k^3) | matrix pack(3, apply(X, i -> if member(i, s) then 1_k else 0_k))));
elapsedTime IL = isoTypes L; -- ~3s with DFS strategy in isomorphism, ~4s with old (fixed) strategy
assert(#IL == 22)

L6 = select(allMatroids 6, M -> rank M <= 3);
binary6 = select(L6, M -> searchRepresentation(M, k) =!= null);
assert(#binary6 == 45 and #select(binary6, M -> rank M == 3) == 22)
all(select(binary6, M -> rank M == 3), M -> any(IL, N -> areIsomorphic(M, N)))

M = (allMatroids(3, 6))#14
N = matroid({{1,2,3},{0,1,2,4},{0,3,4},{1,2,5},{3,5},{0,4,5}}, EntryMode => "circuits")
assert(areIsomorphic(M, N) and areIsomorphic(N, M))

M = (allMatroids(3, 6))#24
assert(#bases M == 4 and #loops M == 2)
U34 = uniformMatroid(3,4)
U02 = uniformMatroid(0,2)
assert(areIsomorphic (U02++U34,U34++U02) and areIsomorphic (U34++U02,U02++U34))
assert(areIsomorphic(M, U34 ++ U02) and areIsomorphic(U34 ++ U02, M))
///

-- Regression for what was previously "TODO: reducedRowEchelonForm does not
-- work with initial zero rows => cannot compute induced representation for
-- e.g. (matroid completeGraph 4) / set{0,1,3}".
-- The induced representation is now correctly computed via the
-- dual-deletion-dual chain in contraction (Matroid, Set).
TEST ///
M = matroid completeGraph 4
N = M / set{0,1,3}
assert(rank N === 1)
assert(N.cache.?storedRepresentation)
A = getRepresentation N
assert(matroid A == N) -- induced representation is consistent with N
-- Same property holds for every 3-element contraction of M
assert all(subsets(toList M.groundSet, 3), s -> (
    NN := M / set s;
    NN.cache.?storedRepresentation and (numrows NN.cache.storedRepresentation == 0 or matroid NN.cache.storedRepresentation == NN)
))
-- And for every 2-element contraction of Fano (so reducedRowEchelonForm is
-- exercised over GF(2) as well as QQ)
F7 = specificMatroid "fano"
assert all(subsets(toList F7.groundSet, 2), s -> (
    NN := F7 / set s;
    NN.cache.?storedRepresentation and matroid NN.cache.storedRepresentation == NN
))
-- And for matrices with trailing zero rows in the stored representation,
-- dual still produces a valid induced representation.
A = matrix{{1_QQ,0,1,1},{0,1,1,1},{0,0,0,0}}
M = matroid A
D = dual M
assert(D.cache.?storedRepresentation)
assert(matroid D.cache.storedRepresentation == D)
///

-- Connectivity sub-features: getSeparation, seriesConnection,
-- parallelConnection, sum2.  Previously only is3Connected was tested.
TEST ///
-- getSeparation
-- K_4 is 3-connected, so no 2-separation exists
assert(getSeparation(matroid completeGraph 4, 2) === null)
-- K_5 is 3-connected but has 3-separations
sep = getSeparation(matroid completeGraph 5, 3)
assert(sep =!= null)
assert(rank(matroid completeGraph 5, sep) + rank(dual matroid completeGraph 5, sep) - #sep <= 3 - 1)

-- seriesConnection / parallelConnection are dual constructions
F7 = specificMatroid "fano"
sc = seriesConnection(F7, F7)
pc = parallelConnection(F7, F7)
assert(rank sc === 6 and #sc_* === 13)
assert(rank pc === 5 and #pc_* === 13)
assert(sc == dual parallelConnection(dual F7, dual F7))

-- Series/parallel connection with U(1,2) is a series/parallel extension
M = matroid completeGraph 3
U12 = uniformMatroid(1, 2)
assert(#(seriesConnection(M, U12))_* === #M_* + 1)
assert(#(parallelConnection(M, U12))_* === #M_* + 1)

-- 2-sum: ground set drops by 2 (basepoint identified and contracted)
U24 = uniformMatroid(2, 4)
S = sum2(U24, U24)
assert(#S_* === 2*#U24_* - 2)
-- specificMatroid "R6" is documented as sum2(U(2,4), U(2,4))
assert(specificMatroid "R6" == sum2(U24, U24))

-- sum2 errors when basepoint 0 is a loop in either argument
M0 = matroid({a,b,c}, {{a,b}, {a,c}, {b,c}})
ML = matroid({a,b,c,d}, {{b,c},{b,d},{c,d}}) -- 0 = a is a loop
assert(member(0, loops ML))
assert(try (sum2(ML, M0); false) else true)
///

-- Special matroid families: thetaMatroid, spike, swirl, wheel, whirl.
-- Previously only binarySpike was tested.
TEST ///
-- thetaMatroid n: 2n ground elements, rank n, self-dual
T3 = thetaMatroid 3
assert(rank T3 === 3 and #T3_* === 6)
assert(areIsomorphic(T3, dual T3))
T4 = thetaMatroid 4
assert(rank T4 === 4 and #T4_* === 8)
assert(areIsomorphic(T4, dual T4))

-- spike r: tipped r-spike has 2r+1 ground elements, rank r
S3 = spike 3
assert(rank S3 === 3 and #S3_* === 7)
-- The free spike has exactly r length-3 circuits (the tip+leg circuits)
assert(#select(circuits S3, c -> #c === 3) === 3)

-- binarySpike r: same shape as spike r but with additional 3-circuits
B3 = binarySpike 3
assert(rank B3 === 3 and #B3_* === 7)
assert(#select(circuits B3, c -> #c === 3) > #select(circuits S3, c -> #c === 3))

-- swirl r: free rank-r swirl has 2r ground elements, rank r
Sw3 = swirl 3
assert(rank Sw3 === 3 and #Sw3_* === 6)

-- wheel r: r=2 has a parallel pair (not pure graphic); r>=3 uses the wheel graph
W2 = wheel 2
W3 = wheel 3
assert(rank W2 === 2 and #W2_* === 4)
assert(rank W3 === 3 and #W3_* === 6)
assert(instance(getRepresentation W3, Graph))
-- W_3 has 6 elements, M(W_3) = M(K_4)
assert(areIsomorphic(W3, matroid completeGraph 4))

-- whirl r: relaxation of wheel r — adds exactly one basis
Wh3 = whirl 3
assert(rank Wh3 === 3 and #Wh3_* === 6)
assert(#bases Wh3 === #bases W3 + 1)
-- The whirl W^3 is the canonical non-binary matroid: it has no GF(2) rep.
assert(searchRepresentation(Wh3, GF 2) === null)
///

-- Modular cut / extension / quotient family.  Previously NONE of extension,
-- coextension, elementaryQuotient, isQuotient, isElementaryQuotient,
-- modularCut, isModularCut, isLinearSubclass, linearSubclass were tested.
TEST ///
-- Doc example for elementaryQuotient
A = matrix {{1_QQ, 0, 0, 1, 1}, {0, 1, 0, 1, -1}, {0, 0, 1, 0, 0}}
M = matroid A
K = {{2}, {2, 4}, {2, 3}, {1, 2}, {0, 2}, {0, 1, 2, 3, 4}}
assert(isModularCut(M, K))
H = linearSubclass(M, K)
assert(set H === set {set{1,2}, set{2,3}, set{4,2}, set{0,2}})
assert(isLinearSubclass(M, H))
-- modularCut from a linear subclass recovers the full cut
assert(set((modularCut(M, H))/set) === set(K/set))

-- elementaryQuotient via modular-cut entry mode and via hyperplanes entry mode
-- must agree.  This used to silently disagree because of a typo
-- ("hyerplanes") and a set-of-lists vs set-of-sets mismatch in extension's
-- K' construction: the modular-cut entry mode included *every* hyperplane
-- in the new bases, producing a matroid with too many bases (no loop at the
-- distinguished element).
Q1 = elementaryQuotient(M, K)
Q2 = elementaryQuotient(M, H, EntryMode => "hyperplanes")
assert(Q1 == Q2)
assert(rank Q1 === rank M - 1)
assert(loops Q1 === {2}) -- element 2 is the new loop
assert(isElementaryQuotient(Q1, M))
assert(isQuotient(Q1, M))
-- modularCut(Q1, M) recovers K
assert(set((modularCut(Q1, M))/set) === set(K/set))

-- extension and coextension are dual constructions
F7 = specificMatroid "fano"
assert(coextension F7 == dual extension dual F7)
-- For uniform matroid U(r,n), the free coextension is U(r+1, n+1)
assert(areIsomorphic(coextension uniformMatroid(2, 4), uniformMatroid(3, 5)))
-- The free extension of U(r,n) is U(r, n+1)
assert(areIsomorphic(extension uniformMatroid(2, 4), uniformMatroid(2, 5)))

-- Trivial modular cut = {groundSet} gives the free extension
M = uniformMatroid(2, 4)
assert(isModularCut(M, {M.groundSet}))
assert(areIsomorphic(extension(M, {M.groundSet}), uniformMatroid(2, 5)))
-- Corresponding elementary quotient: rank drops by 1
EQ = elementaryQuotient(M, {M.groundSet})
assert(rank EQ === rank M - 1)
assert(isElementaryQuotient(EQ, M))

-- isLinearSubclass: empty list is a (vacuous) linear subclass, and a single
-- hyperplane is always a linear subclass
assert(isLinearSubclass(M, {}))
assert(isLinearSubclass(M, {first hyperplanes M}))

-- CheckWellDefined: valid input passes, invalid input errors
A = matrix {{1_QQ, 0, 0, 1, 1}, {0, 1, 0, 1, -1}, {0, 0, 1, 0, 0}}
M = matroid A
assert(try (extension(M, K, CheckWellDefined => true); true) else false)
assert(try (extension(M, drop(K, 1), CheckWellDefined => true); false) else true)
///

-- idealOrlikSolomonAlgebra (true dark export — was exported but neither
-- documented nor in undocumented{}).  The Orlik-Solomon algebra of a matroid
-- M of rank r is a skew-commutative quotient whose Hilbert series
-- coefficients are the |Whitney numbers of the first kind|, i.e. up to sign
-- the coefficients of the reduced characteristic polynomial of M.
TEST ///
-- For U(2, 3) = matroid completeGraph 3, char poly = x^2 - 3x + 2,
-- and OS Hilbert poly = 1 + 3T + 2T^2
M = matroid completeGraph 3
I = idealOrlikSolomonAlgebra M
R = ring I
assert(numgens R === #M_*)
assert(isSkewCommutative R)
A = R/I
hs = numerator hilbertSeries(A, Order => rank M + 1)
T = (ring hs)_0
assert(hs == 1 + 3*T + 2*T^2)

-- For Fano (rank 3, 7 elements): the ideal is generated by one element per
-- length-3 circuit (Fano has 7 such)
F7 = specificMatroid "fano"
I = idealOrlikSolomonAlgebra F7
assert(numgens ring I === 7)
assert(isSkewCommutative ring I)
assert(#(first entries gens I) === 7)

-- Options CoefficientRing and Variable take effect on the ambient ring
I3v = idealOrlikSolomonAlgebra(M, CoefficientRing => ZZ/5, Variable => "f")
assert(coefficientRing ring I3v === ZZ/5)
assert(toString first gens ring I3v === "f_0")
///

-- saveMatroid / readFromFile round-trip.  Previously neither was tested.
TEST ///
M = matroid completeGraph 4
fn = temporaryFileName()
saveMatroid(M, fn)
assert(fileExists fn)
M' = readFromFile fn
assert(instance(M', Matroid))
assert(M == M')

-- Round-trip a non-graphic matroid as well
F7 = specificMatroid "fano"
fn2 = temporaryFileName()
saveMatroid(F7, fn2)
F7' = readFromFile fn2
assert(F7 == F7')
assert(circuits F7 === circuits F7')

-- writeToString produces a string
s = writeToString F7
assert(instance(s, String))
///
