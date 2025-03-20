
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
-- elapsedTime assert all(isos, phi -> areIsomorphic(N, relabel_N phi)) -- ~ 420 seconds
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
