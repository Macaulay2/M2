-- Goal: 
-- determine the complexity of the multihomogeneous trace test,
-- i.e., the degree of the curve      
restart
needsPackage "ReactionNetworks"
needsPackage "NumericalAlgebraicGeometry"
FF = ZZ/32003
-*
FF = CC_53
*-

-*
CRN = reactionNetwork "A <--> 2B, A+C<-->D, B+E-->A+C, D-->B+E"
-- V(I) has two components: principal one of deg=22 
-- V(J) has two components: principal one of deg=15 
*-

CRN = wnt()

S = createRing(CRN, FF);
CEforms := matrix{conservationEquations(CRN,FF)};
n = #CRN.Species
setRandomSeed 0
ccSubList := apply(toList(n..2*n-1), i -> S_i => random FF);
xxSubList := apply(n, i -> S_i => last ccSubList#i);
CE := sub(CEforms, ccSubList);    
SSE := transpose steadyStateEquations CRN
I = ideal (CE|SSE)
Rt = FF[take(gens S, n),t]
toCurve = map(Rt,S,take(gens Rt,n)|(ccSubList/last)|apply(numgens S - 2*n,i->t-random FF))
J = toCurve I 
-* NUMERICAL DECOMPOSITION
J = ideal apply(numgens C - 1, i->sub(random(1,FF[gens C])+1,ring I)) + I;
R = CC[xxx_1..xxx_(numgens ring J)]
toR = map(R,ring J, vars R)
NID = numericalIrreducibleDecomposition(toR J, Software=>BERTINI)
*-

-* SYMBOLIC DECOMPOSITION
decI = decompose I;
decI / degree
*-

gbTrace = 3
-*
degree I
*-
degree J
