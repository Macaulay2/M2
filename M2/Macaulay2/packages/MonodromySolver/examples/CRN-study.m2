restart
needsPackage "ReactionNetworks"
needsPackage "NumericalAlgebraicGeometry"
FF = ZZ/32003
{*
FF = CC_53
*}
CRN = reactionNetwork "A <--> 2B, A+C<-->D, B+E-->A+C, D-->B+E"
CRN = wnt()
S = createRing(CRN, FF);
CEforms := matrix{conservationEquations(CRN,FF)};
SubList := apply(toList(0..numgens S-1), i -> (gens S)#i => random FF);
CE := sub(CEforms, SubList) - CEforms;    
SSE := matrix {steadyStateEquations CRN};
C = coefficientRing S	       	   
I = ideal sub(CE|SSE, FF[gens C,gens S])
{*
J = ideal apply(numgens C - 1, i->sub(random(1,FF[gens C])+1,ring I)) + I;
R = CC[xxx_1..xxx_(numgens ring J)]
toR = map(R,ring J, vars R)
NID = numericalIrreducibleDecomposition(toR J, Software=>BERTINI)
*}
decI = decompose I;
decI / degree
